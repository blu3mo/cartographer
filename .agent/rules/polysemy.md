---
trigger: glob
description: 「Polysemy」を完全に掌握し、実用的な実装から高度な抽象化までを完璧に使いこなすための、超高解像度かつ網羅的な解説
globs: backend/**
---

このドキュメントは、Polysemyの型システム、エフェクトの定義、インタープリタの階層（一次および高階）、そしてパフォーマンスチューニングに至るまで、ソースコードの深部に基づいて構成されています。

---

# Polysemy 完全網羅ガイド：AIコーディングエージェントのための高解像度リファレンス

Polysemyは、Haskellにおける「Higher-Order, Low-Boilerplate Free Monads（高階・低ボイラープレートなFreeモナド）」ライブラリです。MTL（Monad Transformer Library）や従来のFreeモナド（Freer-Simple等）の問題点（$O(n^2)$のインスタンス問題、型推論の弱さ、ボイラープレートの多さ）を解決し、ビジネスロジックと実装詳細を完全に分離するために設計されています。

## 1. コア・アーキテクチャと型システム

Polysemyの中核は `Sem` モナドとエフェクトのリスト（Effect Row）にあります。

### 1.1 Sem モナドと Effect Row
すべての計算は `Sem r a` 型で表現されます。
*   **`r` (Effect Row)**: 型レベルのリスト（`[Effect]`）で、この計算が利用可能な「能力（Capabilities）」を保持します。
*   **`a`**: 計算の結果の型です。

```haskell
data Sem r a
```


プログラムは具体的なエフェクトのリスト（例: `[State String, Error Bool]`) に対して書くのではなく、多相的な `r` に対して `Member` 制約を用いて記述します。これにより、インタープリタの実装を後から自由に差し替えることが可能になります。

### 1.2 Member と Members 制約
特定の機能を使用するために、`r` の中に特定のエフェクトが含まれていることを要求します。

*   **`Member e r`**: エフェクト `e` がリスト `r` に含まれていることを示します。
*   **`Members '[e1, e2] r`**: 複数のエフェクトが含まれていることを一度に記述する糖衣構文です。

```haskell
-- 使用例: ログ出力とKVストアのエフェクトを持つ関数
foo :: Members '[Log, KVStore k v] r => Sem r ()
```

## 2. エフェクトの定義（代数的データ型としての定義）

Polysemyでは、エフェクトを GADT（一般化代数的データ型）として定義します。エフェクトには「一次エフェクト（First-Order）」と「高階エフェクト（Higher-Order）」の2種類があります。

### 2.1 エフェクトの構造
エフェクト型 `E` は `E (m :: * -> *) a` の形をとります。
*   **`m`**: そのエフェクトが埋め込まれている `Sem` モナド自体を表します（再帰的な計算や、コールバックを含むエフェクトに必要）。
*   **`a`**: そのアクションが返す値の型です。

### 2.2 Template Haskell によるボイラープレートの削減
`makeSem` を使用することで、スマートコンストラクタを自動生成できます。これにより、`send` 関数を手動で呼び出す手間が省けます。

**例: キーバリューストア（一次エフェクト）**
```haskell
{-# LANGUAGE TemplateHaskell, GADTs, ScopedTypeVariables, PolyKinds #-}

import Polysemy

data KVStore k v m a where
  GetKV :: k -> KVStore k v m (Maybe v)
  PutKV :: k -> v -> KVStore k v m ()

makeSem ''KVStore
-- これにより、以下の関数が自動生成される:
-- getKV :: Member (KVStore k v) r => k -> Sem r (Maybe v)
-- putKV :: Member (KVStore k v) r => k -> v -> Sem r ()
```


## 3. インタープリタ（解釈系）の深層

Polysemyの真価は、定義したエフェクトをどう「解釈（Handle）」するかにあります。インタープリタはエフェクトをスタックから取り除き、別のエフェクトや最終的な値に変換します。

### 3.1 一次エフェクトの解釈 (`interpret`)
エフェクトが `m` パラメータを使用しない（単なるデータ操作のような）場合、`interpret` を使用します。

```haskell
interpret :: FirstOrder e "interpret"
          => (forall rInitial x. e (Sem rInitial) x -> Sem r x)
          -> Sem (e ': r) a
          -> Sem r a
```


**解説**: `e` を消費して、残りのエフェクト `r` の言葉で表現された計算 `Sem r` に変換します。

### 3.2 状態を持つ解釈 (`reinterpret`)
エフェクトを処理する際、新たなエフェクト（例えば `State`）を導入して処理し、その場では処理しきらないパターンです。これは `State` エフェクトなどを「注入」するのに使われます。

```haskell
-- KVStore を State (Map k v) に変換して解釈する例
runKVStoreInState :: Ord k => Sem (KVStore k v ': r) a -> Sem (State (Map k v) ': r) a
runKVStoreInState = reinterpret $ \case
  GetKV k   -> gets (Map.lookup k)
  PutKV k v -> modify (Map.insert k v)
```

### 3.3 高階エフェクトの解釈 (`interpretH` と Tactics)
エフェクトが `m` を引数に取る場合（例: `Exception` の `catch` や `bracket` のように、計算自体を引数にとる場合）、**Tactics** と呼ばれる特別な機構を使用します。これは Polysemy の最も強力かつ複雑な部分です。

**高階エフェクトの例 (Resource)**:
```haskell
data Resource m a where
  Bracket :: m a -> (a -> m ()) -> (a -> m b) -> Resource m b
```


**解釈の戦略**:
高階エフェクトの解釈には、他のエフェクトの状態を正しくスレッド化（引き継ぎ）する必要があります。
*   `runT`: 引数として渡された計算 `m a` を実行します。
*   `bindT`: 継続（callback）をバインドします。
*   `pureT`: 単純な値を持ち上げます。

```haskell
-- interpretH を使用した高階エフェクトの解釈パターン
interpretH $ \case
  Bracket alloc dealloc use -> do
    -- アクションを現在のコンテキストで実行可能にする
    alloc'   <- runT alloc
    dealloc' <- bindT dealloc
    use'     <- bindT use
    -- これらを使って実装を行う（詳細は省略）
    ...
```


### 3.4 実行順序と状態のセマンティクス
インタープリタを適用する順序は、プログラムの動作（特にエラー処理と状態保持の相互作用）に決定的な影響を与えます。

*   **Error を先に処理**: エラーが発生しても、その時点までの State の変更は破棄される可能性がある（ロールバック的挙動）。
*   **State を先に処理**: エラーが発生しても、State の変更は保持される。

**例**:
```haskell
-- Stateの処理が先（Stateの効果がErrorの影響を受けない）
program & evalState ... & runError
-- Errorの処理が先（エラー時にStateが失われる可能性がある）
program & runError & evalState ...
```


## 4. 外部世界との相互作用

### 4.1 Embed による IO の持ち上げ
`Sem` モナド内で通常の `IO` アクションを行うには、`Embed IO` エフェクトを使用します。アプリケーションコードで直接 `embed` を使うことは推奨されませんが、インタープリタ内では必須です。

```haskell
teletypeToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret $ \case
  ReadTTY      -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg
```


### 4.2 Final による究極の統合
`Final` エフェクトは `Embed` よりも強力で、モナドトランスフォーマースタックの最下層にあるモナド（通常は IO）の機能をフルに活用するために使われます。特に、非同期例外処理や複雑なリソース管理など、Polysemyの標準的なスコープ外の動作を実装する際に必要となります。

```haskell
-- IO を最終的なターゲットとして実行
runM :: Monad m => Sem '[Embed m] a -> m a
```


## 5. パフォーマンスと最適化

Polysemyは「ゼロコスト」を目指していますが、実際にはGHCの最適化に強く依存します。大規模なプロジェクトでは、以下の設定が必須です。

### 5.1 必須のGHCオプション
Polysemyのコードを高速に動作させる（辞書渡しを最適化で消去する）ために、`package.yaml` や `.cabal` に以下を追加する必要があります。
*   `-O2`
*   `-flate-specialise`
*   `-fspecialise-aggressively`

### 5.2 polysemy-plugin の利用
型推論を補助し、パフォーマンスを向上させるために、`polysemy-plugin` を使用することが強く推奨されます。これにより、MTLと同等の推論能力が得られます。

```haskell
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
```

## 6. 実装パターンとベストプラクティス

### 6.1 依存性の注入 (Dependency Injection)
Polysemyは、実装の詳細（DB接続、外部API等）をインタープリタに追い出すことで、DIを型レベルで実現します。テスト時には、IOを伴わない「Pure」なインタープリタに差し替えるだけで、モックテストが可能になります。

### 6.2 既存のモナドスタックからの移行
`Final` エフェクトや `App` モナドパターンを使用することで、既存の MTL ベースのアプリケーションを段階的に Polysemy に移行できます。`Member (Final IO) r` 制約を使用することで、従来の `MonadIO` のような振る舞いを模倣しつつ、徐々にエフェクトへ切り出すことが可能です。

### 6.3 ドメインモデリング
`polysemy-methodology` のようなライブラリを使用すると、ドメインロジックを入力（Input）、処理（Methodology）、出力（Output）のエフェクトとしてモデル化し、それを組み合わせてプログラム全体を構成するという、代数的なアプローチが可能になります。

---

このリファレンスは、Polysemy 1.9.2.0 以降の仕様に基づいています。AIエージェントは、`Sem` モナドの構造、`interpret` による解釈、そして `Tactics` を用いた高階エフェクトの処理を理解することで、型安全かつ柔軟なHaskellアプリケーションを構築できます。

提供されたドキュメントから、Polysemyライブラリの核となる概念、型、関数を重要な点に絞って抜粋・整理する。

### 1. Core Types (核となる型)

* **`Sem r a`**: 拡張可能なEffectのモナド。`r`はEffectのリスト(Row)、`a`は結果の型。
    * mtlとは異なり、同じEffectを複数持つことが可能。
    * 解釈(Interpret)の順序が重要。状態操作や制御フローに影響する(例: `Error`処理と`State`処理の順序で、エラー時の状態保持が変わる)。
* **`Member (t :: Effect) r`**: Effect `t` がスタック `r` に存在することを示す制約。
* **`Members es r`**: 複数の `Member` 制約をまとめる便利機能。
* **`Embed m`**: 一般的なモナド `m` (IOなど) を `Sem` に持ち込むためのEffect。
    * 通常は `embed` 関数で持ち込む。アプリケーションコードでの直接使用は避け、Interpreter内で使うことが推奨される。
* **`Final m`**: Higher-Order Effectを最終的なモナド `m` で解釈するための強力なEffect。
    * `Embed` より強力だが柔軟性は低い。自作Interpreter作成時に有用。

### 2. Running Sem (実行・抽出)

Effectスタックを処理し、最終的な値を取り出す関数群。

* **`run :: Sem '[] a -> a`**: Effectがない純粋な値を抽出。
* **`runM :: Monad m => Sem '[Embed m] a -> m a`**: `Embed m` だけが残った状態からモナド `m` へ降ろす。
* **`runFinal :: Monad m => Sem '[Final m] a -> m a`**: `Final m` だけが残った状態からモナド `m` へ降ろす。

### 3. Stack Manipulation (スタック操作)

* **`raise :: Sem r a -> Sem (e ': r) a`**: Effectをスタックのトップに導入(mtlの `lift` 相当)。
* **`raiseUnder`**: スタックの2番目にEffectを導入。Reinterpretation時に有用。(`raiseUnder2`, `raiseUnder3` もあり)
* **`subsume :: Member e r => Sem (e ': r) a -> Sem r a`**: 重複したEffectを既存の同型Effectとして処理(統合)する。
* **`insertAt`**: 指定したインデックスにEffectを挿入。

### 4. Creating New Effects (Effectの定義)

* **GADT定義**: `data MyEffect m a where ...` の形式で定義。`m` は `Sem r` に相当。
* **`makeSem` / `makeSem_`**: Template Haskellを用いてスマートコンストラクタを自動生成。`_`版は型署名を生成せず、ドキュメント付与に使える。
* **`send`**: スマートコンストラクタを使わず、手動でEffectのアクションを実行する関数。

### 5. Interpreters: First-Order (1階Effectの解釈)

`m` パラメータを使わない(サブ計算を含まない)単純なEffect用。

* **`interpret`**: Effect `e` を消費し、`r` 内の他のEffectに変換(自然変換)する。
* **`intercept`**: Effect `e` を消費せず、処理を挟み込む(ミドルウェア的用途)。
* **`reinterpret`**: Effect `e1` を新しいEffect `e2` に変換して導入する。(`reinterpret2`, `reinterpret3` もあり)
* **`rewrite`**: `e1` を `e2` に書き換え、スタックのトップに置く。
* **`transform`**: `e1` をスタック内の既存の `e2` に変換する。

### 6. Interpreters: Higher-Order & Tactics (高階Effectの解釈)

`m` パラメータ(サブ計算)を含むEffect用。状態 `f` (Functor) のスレッディングが必要。

**Combinators:**
* **`interpretH`**, **`interceptH`**, **`reinterpretH`**: First-Order版のHigher-Order対応版。ハンドラは `Tactical` 環境で動作する。

**Tactics API (`Tactical` 環境内で使用):**
* **`Tactical e m r x`**: Higher-Order Effectの状態 `f` を明示的にスレッディングするための環境。
* **`runT :: m a -> Sem ... (f a)`**: Effectの引数にある `m a` (サブ計算) を実行する。
* **`bindT :: (a -> m b) -> ...`**: 継続 `a -> m b` をリフトする。
* **`pureT :: a -> ...`**: 純粋な値を `Tactical` 環境にリフトする。
* **`getInspectorT`**: 内部状態 `f` を覗き見るための `Inspector f` を取得。コールバック処理などで `Maybe` 値を取り出すのに使う。

### 7. Other Utilities

* **Type Synonyms**:
    * `InterpreterFor e r`: `Sem (e ': r) a -> Sem r a` のエイリアス。
    * `InterpretersFor es r`: 複数のEffect用。
* **Instances**: `Functor`, `Applicative`, `Monad`, `MonadIO`, `MonadFail`, `MonadFix`, `Alternative`, `MonadPlus` 等の標準的なインスタンスは `Polysemy.Internal` で定義済み。
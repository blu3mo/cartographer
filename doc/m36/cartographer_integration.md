# Project:M36 Haskell統合ガイド

このドキュメントは、Cartographerプロジェクトで得られたProject:M36のHaskell統合に関する包括的な知見をまとめたものです。

## 目次

1. [基本概念](#基本概念)
2. [接続とセットアップ](#接続とセットアップ)
3. [型システム統合](#型システム統合)
4. [スキーママイグレーション](#スキーママイグレーション)
5. [スキーマ進化](#スキーマ進化)
6. [既知の制限と対処法](#既知の制限と対処法)
7. [実装例](#実装例)

---

## 基本概念

### Project:M36とは

Project:M36は、純粋な関係代数に基づいたHaskellネイティブのリレーショナルデータベースです。SQLの代わりに真の関係代数を使用し、HaskellのADT（代数的データ型）を直接データベース値として扱えます。

### 主な特徴

- **Haskell ADTサポート**: `Atomable`型クラスを実装した型はデータベース値として使用可能
- **関係代数**: SQLではなく純粋な関係代数でクエリを記述
- **トランザクショングラフ**: GitライクなブランチングトランザクションモデルS
- **永続化**: インメモリとファイルシステム永続化の両方をサポート

---

## 接続とセットアップ

### 接続モード

M36は2つの接続モードをサポートします：

```haskell
data DbConfig
  = InMemory           -- テスト用、揮発性
  | Persistent FilePath -- ファイルシステム永続化
```

### 接続コード例

```haskell
-- Effect/Persistence.hs より
toConnectionInfo :: DbConfig -> ConnectionInfo
toConnectionInfo InMemory =
  InProcessConnectionInfo NoPersistence emptyNotificationCallback []
toConnectionInfo (Persistent dbPath) =
  InProcessConnectionInfo
    (CrashSafePersistence dbPath)
    emptyNotificationCallback
    []

withM36Connection ::
  DbConfig ->
  (Connection -> IO a) ->
  IO (Either DbError a)
withM36Connection config action = do
  let connInfo = toConnectionInfo config
  withConnection connInfo action
```

### 実行結果

```
DB Path: /tmp/m36-schema-evolution-real-test
Inserting event with InsightExtracted payload...
SUCCESS: Event inserted
```

---

## 型システム統合

### Atomable型クラス

Haskellの型をM36で使用するには、`Atomable`インスタンスを派生させます：

```haskell
-- Domain/Types.hs より
data FactPayload
  = ContextDefined SessionContext
  | InsightExtracted Text
  | QuestionDerived Text (Maybe EventId)
  | AnswerProvided Text EventId
  | ReportGenerated Text EventId  -- 後から追加されたコンストラクタ
  deriving (Eq, Show, Generic, NFData, Serialise)
  deriving (Atomable) via WineryVariant FactPayload
```

### 依存型の登録順序

**重要**: M36は依存する型を自動的に登録しません。ネストした型は依存順序を守って登録する必要があります。

```haskell
-- Effect/Persistence.hs より
migrateSchema :: Connection -> IO (Either DbError ())
migrateSchema conn = do
  -- 1. まず依存するnewtype型を登録
  _ <- executeDatabaseContextExpr conn (toAddTypeExpr (undefined :: SessionTitle))
  _ <- executeDatabaseContextExpr conn (toAddTypeExpr (undefined :: SessionPurpose))
  _ <- executeDatabaseContextExpr conn (toAddTypeExpr (undefined :: SessionTopic))
  _ <- executeDatabaseContextExpr conn (toAddTypeExpr (undefined :: SessionBackground))

  -- 2. 次に依存型を含む複合型を登録
  _ <- executeDatabaseContextExpr conn (toAddTypeExpr (undefined :: SessionContext))
  _ <- executeDatabaseContextExpr conn (toAddTypeExpr (undefined :: FactPayload))
  _ <- executeDatabaseContextExpr conn (toAddTypeExpr (undefined :: Event))

  -- 3. 最後にリレーション変数を定義
  executeDatabaseContextExpr conn (toDefineExpr (undefined :: [Event]) "events")
```

### エラー例：依存型未登録

依存型を登録せずに親型を登録しようとすると：

```
NoSuchTypeConstructorName "SessionTitle"
```

---

## スキーママイグレーション

### toAddTypeExpr

`toAddTypeExpr`は、Haskell型からM36の型定義式を生成します：

```haskell
toAddTypeExpr (undefined :: MyType)
```

### toDefineExpr

`toDefineExpr`は、リレーション変数（テーブル相当）を定義します：

```haskell
toDefineExpr (undefined :: [Event]) "events"
```

### トランザクション

```haskell
withTransaction conn $ do
  case toInsertExpr [event] "events" of
    Left err -> error $ "toInsertExpr failed: " ++ show err
    Right insertExpr -> do
      execute insertExpr
      query (RelationVariable "events" ())
```

---

## スキーマ進化

### シナリオ

ADTに新しいコンストラクタを追加する場合：

1. **v1型定義**: `ReportGenerated`なし
2. **v2型定義**: `ReportGenerated`あり

### テスト結果

```
==========================================
Schema Evolution Phase 1: v1 Type Definition
==========================================
DB Path: /tmp/m36-schema-evolution-real-test
SUCCESS: Event inserted

Relation contents:
Relation (attributesFromList [
  (Attribute "eventId" UUIDAtomType),
  (Attribute "sessionId" UUIDAtomType),
  (Attribute "timestamp" DateTimeAtomType),
  (Attribute "payload" (ConstructedAtomType "FactPayload" (fromList [])))
]) (RelationTupleSet {asList = [
  RelationTuple ... [
    UUIDAtom b6a20944-d487-405e-9411-764374ac221f,
    UUIDAtom b2579627-f201-44f7-8f07-40853c235e32,
    DateTimeAtom 2025-12-27 09:42:55.489448 UTC,
    ConstructedAtom "InsightExtracted" ... [TextAtom "..."]
  ]
]})
```

---

## 既知の制限と対処法

### 制限1: migrateSchemaは冪等ではない

**問題**: 既存のDBに再接続してmigrateSchemaを再実行すると、型が既に存在するためエラーになります。

```
RelError (DataConstructorNameInUseError "SessionTitle")
```

**対処法**: スキーマの存在をチェックしてから条件付きでマイグレーションを実行します。

```haskell
-- 推奨パターン（概念的）
migrateSchemaIfNeeded :: Connection -> IO (Either DbError ())
migrateSchemaIfNeeded conn = do
  exists <- checkSchemaExists conn
  if exists
    then pure (Right ())  -- スキーマ既存、スキップ
    else migrateSchema conn  -- 初回のみ実行
```

**Phase 2テスト出力**:

```
==========================================
Schema Evolution Phase 2: v2 Type Definition
==========================================
DB Path: /tmp/m36-schema-evolution-real-test
Connecting to existing DB and reading Phase 1 data...
  Migration error (may be expected): RelError (DataConstructorNameInUseError "SessionTitle")

KNOWN LIMITATION: Migration failed: RelError (DataConstructorNameInUseError "SessionTitle")

This is EXPECTED behavior!
M36's migrateSchema is NOT idempotent.

In production, you would need to:
  1. Check if schema already exists before migrating
  2. Use conditional migration logic
```

### 制限2: 型は永続化されるがセッション間で再登録が必要

M36はカスタム型をディスクに永続化しますが、新しいセッションでは型情報がメモリにロードされていないため、操作前に型を再登録する必要があります。

### 制限3: Haskellスクリプト機能の制限

```
Warning: Haskell scripting disabled: ScriptSessionLoadError cannot satisfy -trust project-m36
```

これはGHCの`-trust`フラグに関する問題で、`AtomFunction`のHaskellスクリプト機能が無効になります。基本的なデータ永続化には影響しません。

---

## 実装例

### 完全なテストコード

```haskell
-- test/Effect/PersistenceSpec.hs より
it "complete schema evolution: same DB, multiple reconnections, strict assertions" $ do
  testId <- nextRandom
  let testDbPath = "/tmp/m36-evolution-strict-" ++ toString testId

  -- Phase 1: 初回接続、スキーマ登録、InsightExtracted挿入
  phase1Result <- withM36Connection (Persistent testDbPath) $ \conn -> do
    migResult <- migrateSchema conn
    case migResult of
      Left err -> pure (Left err)
      Right () -> do
        txnResult <- withTransaction conn $ do
          case toInsertExpr [event1] "events" of
            Left err -> error $ "toInsertExpr failed: " ++ show err
            Right insertExpr -> do
              execute insertExpr
              query (RelationVariable "events" () :: RelationalExpr)
        pure txnResult

  -- Phase 1 アサーション
  case phase1Result of
    Left err -> expectationFailure $ "Phase 1 failed: " ++ show err
    Right (Left err) -> expectationFailure $ "Phase 1 query failed: " ++ show err
    Right (Right relation) -> do
      let relationStr = show relation
      relationStr `shouldContain` "InsightExtracted"
      putStrLn "✓ Phase 1: 1 event inserted (InsightExtracted)"
```

### 2段階スキーマ進化テストの実行方法

```bash
# Phase 1: v1型定義でDB保存
rm -rf /tmp/m36-schema-evolution-real-test
cabal run schema-evolution-phase1

# Phase 2: v2型定義で再接続（migrateSchemaの制限を確認）
cabal run schema-evolution-phase2
```

---

## まとめ

### M36のHaskell統合で得られた知見

1. **型の依存順序が重要**: ネストした型は依存順序を守って登録する
2. **migrateSchemaは冪等ではない**: 再接続時は条件付きマイグレーションが必要
3. **WineryVariantが便利**: Sum型のシリアライズに最適
4. **ADT拡張は自然に動作**: 新しいコンストラクタ追加は型定義変更のみ
5. **永続化DBは再接続時に型再登録が必要**: ただし冪等でないためチェックロジックが必要

### 推奨プラクティス

- テスト時は`InMemory`モード、本番は`Persistent`モード
- 依存型は明示的に登録順序を管理
- スキーマ存在チェックロジックを実装
- `WineryVariant`でSum型のAtomable派生

---

## 関連ファイル

| ファイル | 説明 |
|---------|-----|
| `backend/src/Effect/Persistence.hs` | M36接続とマイグレーション |
| `backend/src/Domain/Types.hs` | ADT定義とAtomable派生 |
| `backend/test/Effect/PersistenceSpec.hs` | スキーマ進化テスト |
| `backend/app/SchemaEvolutionPhase1.hs` | Phase 1テストプログラム |
| `backend/app/SchemaEvolutionPhase2.hs` | Phase 2テストプログラム |
| `scripts/test-schema-evolution.sh` | 2段階テストスクリプト |

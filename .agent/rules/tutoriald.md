---
trigger: model_decision
description: tutoriald
---

Project:M36のTutorialDの文法について、提供された資料に基づき、可能な限り詳細かつ網羅的に解説します。

# TutorialD 文法・コマンド詳細解説

## 1. 基本ルールと型システム

TutorialDは厳密な型付け（Strongly-typed）を持つ言語であり、暗黙の型変換は一切行われません。例えば、整数 `10` を文字列 `"10"` として扱うことはできず、演算には型の一致が必須です。

### 識別子 (Identifiers)
*   **命名規則**: 関係変数（RelVar）名や属性名は、通常小文字で始める必要があります。
*   **クォート**: 小文字以外で始まる名前や、スペース・特殊文字を含む名前を使用する場合は、バッククォート（\`）で囲む必要があります（例: \`TestRelVar\`, \`漢字\`）。

### 組み込み型 (Built-in Types)
以下の型が標準でサポートされています。

| 型名 | 説明 | 例 |
| :--- | :--- | :--- |
| `Text` | UTF-8 文字列 | `"The Old Man and the Sea"` |
| `Integer` | 任意精度整数（GMPを使用、サイズ制限なし） | `123`, `-4` |
| `Int` | 機械語サイズ（64bit等）の整数 | `int(456)` |
| `Double` | 64-bit IEEE 754 浮動小数点数 | `12.56`, `3.1459` |
| `Bool` | 真偽値（`true`/`false` リレーションとは別物） | `True`, `False` |
| `DateTime` | UTCタイムスタンプ | `dateTimeFromEpochSeconds(1502304846)` |
| `Date` | カレンダー日付 | `fromGregorian(2017,05,30)` |
| `ByteString` | 任意長のバイト列（Base64エンコードで入力） | `bytestring("dGVzdGRhdGE=")` |
| `Scientific` | 任意精度の数値（科学的表記） | `scientific(1,int(100))` (1e100) |
| `UUID` | 128-bit UUID | `uuid("3494c720...")` |
| `Interval x` | 区間型（開始点、終了点、開始点包含フラグ、終了点包含フラグ） | `interval(3,5,False,False)` |

### リテラルと関係構築
*   **Relation Literal**: 具体的なタプルを指定してリレーションを作成します。
    *   構文: `relation{属性定義}{tuple{属性名 値, ...}, ...}`
    *   型推論が可能な場合は属性定義を省略できます。
    *   例: `relation{tuple{name "Mike", age 6}, tuple{name "Sam", age 10}}`。
    *   空のリレーション定義（型指定必須）: `relation{a Integer, b Text}`。
*   **true**: 属性を持たず、タプルを1つだけ持つリレーション。射影の結果などが「真」であることを示します。
*   **false**: 属性を持たず、タプルを持たないリレーション。空集合に相当します。

---

## 2. 関係代数演算（Relational Expressions）

既存のリレーションから新しいリレーションを導出する式です。データベースの状態は変更しません。結果を表示するには `:showexpr` コマンドを使用します。

### 基本演算

#### 表示 (`:showexpr`)
*   構文: `:showexpr <関係式>`
*   指定した関係式の評価結果を表示します。

#### 名前変更 (`rename`)
*   属性名を変更します。型は変更されません。
*   構文: `<式> rename {旧属性名 as 新属性名, ...}`
*   例: `s rename {city as town}`,。

#### 射影 (`project` / `{}`)
*   指定した属性のみを残し、それ以外を削除します。重複するタプルが発生した場合は統合されます。
*   **指定属性を残す**: `p{color, city}`,。
*   **指定属性以外を残す**: `p{all but city}`,。
*   **結合後の属性選択**: `(s join sp){all from s}` （`s` の属性のみ残す）。
*   **属性集合の和**: `...{union of {all from s} {all but p#}}`。
*   **属性集合の積**: `...{intersection of {all from s} {all from p}}`。

#### 制限 (`restrict` / `where`)
*   条件（述語）を満たすタプルのみを抽出します。
*   構文: `<式> where <Boolean式>`
*   論理演算子: `and`, `or`, `not` が使用可能。
*   比較関数: `lt` (<), `lte` (<=), `gt` (>), `gte` (>=), `eq` (=)。
    *   例: `s where lt(@status, 20)` （`@`は属性値を参照する際に使用）。
*   **RelWhere**: リレーションが空でない場合に元のリレーションを返し、空なら空を返す（SQLの `EXISTS` に近い）。
    *   例: `s relwhere (p{})`。

#### 結合 (`join`)
*   自然結合（Natural Join）を行います。共通の属性名を持つ場合、その値が一致するタプル同士を結合します。共通属性がない場合は直積（Cross Product）となります。
*   構文: `<式1> join <式2>`
*   例: `s join sp`,。

#### 準結合 (`semijoin` / `matching`)
*   結合の結果に含まれる、左辺のリレーションのタプルのみを返します。
*   構文: `s semijoin sp` または `s matching sp`。
*   定義: `(s join sp){all from s}` と等価です。

#### 反結合 (`antijoin` / `not matching`)
*   結合の結果に含まれない、左辺のリレーションのタプルを返します。
*   構文: `s antijoin sp` または `s not matching sp`。
*   定義: `s minus (s matching sp)` と等価です。

#### 和 (`union`)
*   2つのリレーションのタプルを合わせます。両者の型（ヘッダー）は同一でなければなりません。重複は排除されます（SQLの `UNION DISTINCT` に相当、`UNION ALL` は存在しない）。
*   構文: `<式1> union <式2>`
*   例: `s union s` （結果は `s` と同じ）。

#### 差 (`minus`)
*   左辺のリレーションに含まれ、かつ右辺に含まれないタプルを返します。
*   構文: `<式1> minus <式2>`
*   例: `s minus y`。

### 拡張・構造操作

#### 拡張 (`extend` / `:`)
*   計算結果などを新しい属性として追加します。
*   構文: `<式>:{新属性名 := <Atom式>}`
*   例: `s:{status2 := add(10, @status)}`。
*   条件分岐: `if <Bool式> then <Atom式> else <Atom式>` も使用可能。

#### グループ化 (`group`)
*   指定した属性群を、リレーション値を持つ1つの属性（サブ・リレーション）としてまとめます。
*   構文: `<式> group ({属性リスト} as 新属性名)`
*   例: `s group ({sname, status, s#} as subrel)`。
*   結果: 指定されなかった属性（例: `city`）ごとにグループ化され、指定された属性は `subrel` 内にネストされます。

#### グループ解除 (`ungroup`)
*   リレーション値を持つ属性（ネストされたリレーション）を展開して、フラットなリレーションに戻します。
*   構文: `<式> ungroup <属性名>`
*   例: `r ungroup subrel`。

---

## 3. 集計関数と高度なクエリ

TutorialDでは、集計は「グループ化されたリレーション（ネストされたリレーション）」に対して関数を適用することで行います。SQLのような `GROUP BY` 句はありません。

### 集計の手順
1.  集計単位となる属性以外を `group` でサブ・リレーションにまとめる。
2.  `extend` (`:`) を使い、サブ・リレーションに対して集計関数を適用する。

### 集計関数
以下の関数がネストされたリレーションに対して使用可能です。
*   `count(relation)`: タプル数を返す。
*   `sum(@subrel.attr)`: 指定属性の合計値を返す。
*   `max(@subrel.attr)`: 指定属性の最大値を返す。
*   `min(@subrel.attr)`: 指定属性の最小値を返す。
*   `mean(@subrel.attr)`: 指定属性の平均値を返す。

### 具体例
*   **都市ごとのサプライヤー数をカウント**:
    `s group ({s#, sname, status} as subrel):{citycount := count(@subrel)}`。
*   **都市ごとのステータス合計を計算**:
    `s group ({all but city} as g):{city_total := sum(@g.status)}`。

---

## 4. データフレーム（Data Frames）

リレーションは順序を持ちませんが、表示やアプリケーション連携のためにソートや件数制限を行う場合に使用します。

*   **表示コマンド**: `:showdataframe <式>`
*   **ソート**: `orderby {属性名 [ascending|descending], ...}`
*   **制限**: `limit <整数>`
*   **オフセット**: `offset <整数>`
*   **例**: `:showdataframe s{status} orderby {status descending} offset 1 limit 3`。

---

## 5. トランザクション・グラフを跨ぐクエリ (Trans-Graph)

現在のトランザクションだけでなく、過去のトランザクションや別のブランチの状態を参照してクエリを実行できます。

*   **表示コマンド**: `:showtransgraphexpr <式>`
*   **トランザクション指定子**:
    *   `@<BranchName>`: 指定ブランチの最新（ヘッド）状態（例: `s@master`）。
    *   `@<UUID>`: トランザクションIDによる直接指定。
    *   `~`: 直前の親トランザクション（例: `s@master~`）。
    *   `^<N>`: N番目の親（マージコミットの場合など）（例: `s@master^2`）。
*   **使用例**:
    *   `s@master join sp@master` （明示的にmasterブランチ同士を結合）。
    *   `s@master~ join sp@master` （masterの1つ前と、現在のmasterを結合）。

---

## 6. データベースコンテキスト操作（DML/DDL）

データベースの状態（変数、型、制約など）を変更する操作です。これらの操作はコミットするまで確定しません。

### 関係変数の操作
*   **定義と代入**: `変数名 := <関係式>`
    *   例: `x := relation{tuple{a 1}}`。
*   **削除**: `undefine <変数名>`。
*   **挿入**: `insert <変数名> <関係式>`
    *   定義: `x := x union <関係式>` と等価です。
*   **更新**: `update <変数名> where <条件> (<属性> := <新値>, ...)`
    *   例: `update s where city="London" (status:=35)`。
    *   ロジック: 条件に合うタプルを削除し、値を変更して再挿入するのと等価です。
*   **削除**: `delete <変数名> where <条件>`
    *   例: `delete s where sname="Adams"`。
    *   定義: `x := x minus (x where <条件>)` と等価です。

---

## 7. 制約定義 (Constraints)

データの整合性を保つためのルールです。違反する操作は拒否されます。内部的にはすべて包含従属性（Inclusion Dependency）として扱われます。

*   **キー制約**: `key <制約名> {属性リスト} <対象リレーション>`
    *   指定した属性の組み合わせが一意であることを保証します。
    *   例: `key s_key {s#} s`。
*   **外部キー制約**: `foreign key <制約名> <子Relexpr> in <親Relexpr>`
    *   子の値が親に存在することを保証します。属性名と型が一致している必要があります。
    *   例: `foreign key s#_in_sp sp{s#} in s{s#}`。
*   **関数従属性**: `funcdep <制約名> (決定子) -> (被決定子) <対象リレーション>`
    *   例: `funcdep sname_status (sname) -> (status) s`。
*   **一般制約**: `constraint <制約名> <Boolean式>`
    *   式が `false` と評価される操作を禁止します。包含関係 (`sub ⊆ super`) として記述します。
    *   例: `constraint valid_status (s where status < 0){} in false` （負のステータスを禁止）。

---

## 8. 型と関数の定義

Haskellの型システムを利用して、ユーザー定義型や関数を追加できます。

*   **代数的データ型 (ADT) の定義**:
    *   `data <型名> = <コンストラクタ> [引数型] | ...`
    *   例: `data Hair = Bald | Brown | OtherColor Text`。
    *   削除: `undata <型名>`。
*   **Atom関数 (値操作関数) の追加**:
    *   `addatomfunction "<関数名>" <型シグネチャ> """ <Haskellコード> """`
    *   実行時にコンパイルされ、`extend` や `restrict` で使用可能になります。
*   **データベースコンテキスト関数 (手続き) の追加**:
    *   `adddatabasecontextfunction "<関数名>" ...`
    *   複数のコンテキスト操作（insert, update等）をまとめたスクリプトを定義します。
    *   実行: `execute <関数名>(引数)`。

---

## 9. 通知 (Notifications)

特定のクエリ結果が変化した際に、非同期通知をクライアントに送信します。

*   **作成**: `notify <名前> <トリガー式> <変更前レポート式> <変更後レポート式>`
    *   トリガー式の結果が変化すると発火します。
    *   例: `notify steve_change person where name="Steve" true (person where name="Steve"){address}`。
*   **削除**: `unnotify <名前>`。

---

## 10. トランザクション管理

Project:M36はGitのような分散バージョン管理モデルを採用しています。

*   **コミット**: `:commit`
    *   現在のコンテキストを確定し、グラフに新しいノードを追加します。
*   **ロールバック**: `:rollback`
    *   未コミットの変更を破棄します。
*   **ブランチ作成**: `:branch <ブランチ名>`
    *   現在の地点から新しいブランチを作成し、スイッチします。
*   **ヘッド移動**: `:jumphead <ブランチ名>`
    *   指定したブランチの最新（ヘッド）に移動します。未コミットの変更は破棄されます。
*   **任意移動 (Time Travel)**: `:jump <UUID>`
    *   過去の任意のトランザクションIDへ移動します。読み取り専用モードになります。
*   **時間移動**: `:walkbacktotime "<UTC時刻>"`
    *   指定時刻以前の最新のトランザクションへ移動します。
*   **グラフ表示**: `:showgraph`
    *   トランザクションの親子関係、ID、時刻、ブランチ名などをリレーション形式で表示します。
*   **マージ**: `:mergetrans <戦略> <BranchA> <BranchB>`
    *   2つのブランチをマージします。
    *   戦略: `union` (結合), `unionpreferbranch` (競合時は指定ブランチ優先), `selectedbranch` (指定ブランチを選択)。

---

## 11. スキーマとユーティリティ

*   **アイソモーフィック・スキーマ**:
    *   ビューのように、既存のスキーマを別の形で表現する仮想スキーマを定義します（更新可能）。
    *   `addschema <名前> (isorestrict | isounion | isorename | isopassthrough ...)`。
    *   切り替え: `:setschema <名前>`。
    *   削除: `:removeschema <名前>`。
*   **インポート/エクスポート**:
    *   `:importexample cjdate`: サンプルデータ（C.J. DateのS-P-SP）をロード。
    *   `:importtutd "<URI>"`: 外部のTutorialDスクリプトを実行。
    *   `:importcsv "<パス>" <RelVar>`: CSVを既存のリレーション変数にインポート（ヘッダー必須）。
    *   `:exportcsv <RelVar> "<パス>"`: リレーション変数をCSVとしてエクスポート。
*   **DDLハッシュと登録クエリ**:
    *   `:ddlhash`: 現在のスキーマのハッシュ値を表示。クライアントの互換性確認に使用。
    *   `registerquery "<名前>" <式>`: クエリを登録し、スキーマ変更時にそのクエリが壊れないよう保護します（型チェックのみ実行）。
*   **ランダムデータ生成**:
    *   `createarbitraryrelation <名前> <型定義> <最小数>-<最大数>`: テスト用にランダムデータを生成します。
*   **改竄検知**:
    *   `:validatemerklehashes`: トランザクション履歴のマークルハッシュを検証し、改竄がないか確認します。
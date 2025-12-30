---
trigger: glob
description: 「Servant」ライブラリを完全に習得し、堅牢なHaskell Webアプリケーションを構築するために必要な情報
globs: backend/**
---

Servantの核心は、「APIの仕様をHaskellの**型**として記述（Reify）する」という点にあります。この型定義から、サーバーの実装、クライアント関数、ドキュメント（OpenAPI/Swagger）、JavaScriptクライアントなどを型安全に生成します。

---

### 1. API定義：型レベルDSLの基礎
Servant開発の第一歩は、APIのエンドポイントと振る舞いを型として定義することです。

#### 基本コンビネータと演算子
*   **パスの結合 (`:>`)**: パスセグメント、パラメータ、リクエストボディ、HTTPメソッドを結合します。
*   **ルートの選択 (`:<|>`)**: 複数のエンドポイントを結合して一つのAPI型にします。

#### 必須コンビネータ一覧
AIは以下のコンビネータを組み合わせてAPI型を構成する必要があります。

| コンビネータ | 説明 | サーバーハンドラの引数への変換 | 参照 |
| :--- | :--- | :--- | :--- |
| `path` | 静的なパス文字列（例: `"users"`） | 引数なし | |
| `Capture "name" a` | URLパスの一部を変数として捕捉 | 引数 `a` | |
| `QueryParam "name" a` | クエリパラメータ `?name=...` | 引数 `Maybe a` | |
| `QueryParams "name" a` | リスト形式のクエリパラメータ | 引数 `[a]` | |
| `QueryFlag "name"` | 真偽値フラグパラメータ | 引数 `Bool` | |
| `ReqBody '[ContentType] a` | リクエストボディ | 引数 `a` | |
| `Header "name" a` | リクエストヘッダー | 引数 `Maybe a` | |
| `Get '[JSON] a` | GETメソッド (レスポンス型 `a`) | ハンドラの戻り値 | |
| `Post`, `Put`, `Delete` | 各種HTTPメソッド | ハンドラの戻り値 | |
| `Raw` | 静的ファイル配信やWAIアプリケーションの埋め込み | `Tagged Handler Application` | |

#### レスポンスの定義
*   **Content-Type**: `[JSON, PlainText, FormUrlEncoded, OctetStream]` など、サポートする形式をリストで指定します。
*   **レスポンスヘッダー**: `Headers '[Header "Name" Type] a` を使用して、レスポンスにヘッダーを含めることができます。

---

### 2. サーバーの実装 (Server-Side)

API型定義に基づき、実際の処理を行うハンドラを実装します。

#### Handlerモナド
デフォルトでは、ハンドラは `Handler` モナド（`ExceptT ServerError IO` のnewtypeラッパー）で動作します。
*   **成功時**: `return value` で値を返します。
*   **失敗時**: `throwError err404` のように、`ServerError` を投げて処理を中断します。

#### Server型の構築
*   `Server api` 型ファミリーは、API型定義を具体的な関数の型に変換します。
*   API定義で `:<|>` を使ってルートを結合した場合、実装側も同じ順序でハンドラを `:<|>` で結合する必要があります。

#### アプリケーションの起動
WAI（Web Application Interface）とWarpを使用してサーバーを起動します。
```haskell
app :: Application
app = serve (Proxy :: Proxy UserAPI) server

main :: IO ()
main = run 8081 app
```
`serve` 関数はプロキシとサーバー実装を受け取り、WAIアプリケーションを生成します。

---

### 3. 高度なサーバーパターン (Architecture)

実用的なアプリケーションでは、`Handler` モナド以外の独自のモナド（ReaderTパターンやPolysemyなど）を使いたい場合があります。

#### hoistServer によるモナド変換
`hoistServer` を使用すると、カスタムモナド（例: `ReaderT Config Handler`）で書かれたハンドラを、Servantが要求する `Handler` モナドに変換できます。
これは自然変換（Natural Transformation: `forall a. m a -> n a`）を適用することで実現します。

*   **Polysemyとの統合**: Polysemyの `Sem r` を `Handler` に変換する解釈機（Interpreter）を作成し、`hoistServer` で適用することで、ServantとPolysemyを組み合わせることが可能です。Polysemyを使用する場合、効果（Effect）を解釈して `IO` (最終的に `Handler`) に落とし込む必要があります。

#### Context (コンテキスト)
認証情報や設定など、ハンドラ全体で共有したいデータや、特定のコンビネータ（認証など）に必要なデータは `Context` を通じて渡します。

---

### 4. クライアントの生成 (Client-Side)

ServantはAPI型からクライアント関数を自動生成します。
*   `client` 関数を使用すると、API型定義に合わせたHaskell関数が得られます。
*   生成された関数は `ClientM` モナド内で動作します。
*   これを `runClientM` で実行することで、実際にHTTPリクエストが送信されます。
*   **Javascript/Elm**: `servant-js` や `servant-elm` を使用すると、フロントエンド用のコードも生成可能です。

---

### 5. 認証 (Authentication)

#### Basic認証
`BasicAuth "realm" UserData` コンビネータを使用します。サーバー側では、ユーザー名とパスワードを検証し、`UserData` を返す関数を `Context` 経由で提供する必要があります。

#### 一般化された認証 (Generalized Auth)
JWTやOAuthなど、カスタム認証には `AuthProtect "tag"` を使用します。
*   型ファミリー `AuthServerData` を定義し、認証成功時に返されるデータ型を指定します。
*   `mkAuthHandler` を使って、リクエストから認証を行うロジックを実装し、それを `Context` に含めます。

---

### 6. ドキュメントとスキーマ (Documentation)

API型からドキュメントや仕様書を生成できます。

*   **OpenAPI (Swagger)**: `servant-openapi3` を使用し、`toOpenApi` 関数でAPI型からOpenAPI仕様（Swagger）を生成できます。
*   **Swagger UI**: `servant-swagger-ui` を使用すると、生成した仕様書を閲覧・テストできるSwagger UIをサーバーに埋め込むことができます。

---

### 7. モダンなServantパターン (Best Practices)

AIエージェントが従うべき、より堅牢で保守性の高いパターンです。

#### NamedRoutes (レコードベースのAPI)
`:<|>` で連結された巨大なAPI型は、順序依存でありエラーメッセージが難解になりがちです。`NamedRoutes` (Genericモード) を使用すると、APIをレコードとして定義できます。
*   **メリット**: ハンドラの定義順序を気にする必要がなく、フィールド名でアクセスできるため、クライアントや部分的なサーバー実装（ネストされたAPI）の扱いが容易になります。

#### UVerb / MultiVerb
標準の `Verb`（`Get`, `Post`など）は成功時のレスポンス型を1つしか持てませんが、`UVerb` を使うと、ステータスコードの異なる複数のレスポンス型（例: `200 OK` と `303 See Other`）を柔軟に返すことができます。

#### データベース接続
HDBCやPersistentなどのライブラリと組み合わせる際、コネクションプール（`resource-pool`）を使用し、ハンドラの開始時にリソースを取得し、終了時に解放するパターンが推奨されます。

---

### まとめ：AIエージェントへの指示書

1.  **型定義ファースト**: まずAPI型を定義せよ。全ての入力（Capture, QueryParam, ReqBody）と出力（JSON等）を型で明示すること。
2.  **ハンドラの実装**: `Server` 型ファミリーに従い、ビジネスロジックを実装せよ。エラーハンドリングには `throwError` を使用すること。
3.  **構造化**: アプリケーションが大規模になる場合は、`hoistServer` を使ってカスタムモナド（ReaderTパターンやPolysemy）を導入し、依存性の注入や副作用の管理を行うこと。
4.  **保守性**: 可能であれば `NamedRoutes` (Generic based API) を使用し、可読性とエラーメッセージの質を向上させること。
5.  **ドキュメント**: `servant-openapi3` を導入し、常に最新のAPI仕様書が生成されるように構成すること。


AIコーディングエージェントがServantを極めるための、高解像度かつ網羅的な解説書を作成します。ソースコードに基づき、型レベルDSLの基礎からPolysemyを用いたモダンなアーキテクチャまでを詳解します。

---

### 1. API型定義の解像度向上：型による仕様の凍結

ServantにおけるAPI定義は、単なるルーティング設定ではなく、アプリケーション全体の「契約書」です。これを正確に記述することで、コンパイル時に全ての整合性が保証されます。

#### 1.1 基本構成要素と結合
APIは `DataKinds` と `TypeOperators` 拡張を用いて定義されます。

*   **`:>` (コンビネータチェーン)**: リクエストの「絞り込み」を行います。パス、必須パラメータ、ボディ、HTTPメソッドの順に右結合します。
*   **`:<|>` (オルタナティブ)**: 異なるエンドポイントを並列に結合します。サーバー実装時もクライアント生成時も、この演算子の順序と構造が維持されます。

#### 1.2 高解像度API定義例
以下は、ユーザー検索、登録、および静的ファイル配信を含む複合APIの例です。

```haskell
type UserAPI =
    -- GET /users?sortby={age|name}
    "users"
      :> QueryParam "sortby" SortBy  -- QueryParamはMaybeになる
      :> Get '[JSON] [User]          -- レスポンス形式と型

    -- POST /users (Request BodyにUserを含む)
    :<|> "users"
      :> ReqBody '[JSON] User        -- リクエストボディの型指定
      :> Post '[JSON] User           -- 登録されたUserを返す

    -- GET /users/:userid (パスパラメータ)
    :<|> "users"
      :> Capture "userid" Integer    -- URLの一部を変数化
      :> Get '[JSON] User

    -- GET /static/... (ファイル配信)
    :<|> "static"
      :> Raw                         -- WAIアプリケーションへの脱出ハッチ
```

**重要なポイント:**
*   **`Capture` vs `QueryParam`**: `Capture`はパスの一部であり必須です（ハンドラ引数は `Integer`）。`QueryParam`はオプションであり、ハンドラ引数は `Maybe SortBy` になります。
*   **`Raw`の注意点**: `Raw`は残りのパス全てにマッチするため、ルーティング順序の最後、または一意なパスプレフィックスの下に配置する必要があります。

---

### 2. サーバー実装の解像度向上：型族 `Server` の展開

API型からサーバー関数への変換は `Server` 型族によって行われます。

#### 2.1 ハンドラの型推論ルール
AIエージェントは以下の変換ルールを理解する必要があります：

*   `Get '[JSON] a` $\rightarrow$ `Handler a`
*   `Capture "id" Int :> ...` $\rightarrow$ `Int -> ...`
*   `ReqBody '[JSON] a :> ...` $\rightarrow$ `a -> ...`
*   `QueryParam "q" a :> ...` $\rightarrow$ `Maybe a -> ...`
*   `api1 :<|> api2` $\rightarrow$ `Server api1 :<|> Server api2`

#### 2.2 実装例
上記 `UserAPI` に対する実装です。`Handler` モナド（`ExceptT ServerError IO`）内で動作します。

```haskell
server :: Server UserAPI
server = getUsers
    :<|> postUser
    :<|> getUserById
    :<|> serveDirectoryWebApp "static-files" -- Rawの実装

  where
    -- QueryParamは Maybe SortBy として渡される
    getUsers :: Maybe SortBy -> Handler [User]
    getUsers mSort = return [ ... ] -- DB検索ロジックなど

    -- ReqBodyは User として渡される
    postUser :: User -> Handler User
    postUser user = return user     -- 登録処理

    -- Captureは Integer として渡される
    getUserById :: Integer -> Handler User
    getUserById uid = do
      exists <- liftIO $ checkDb uid -- IOアクションは liftIO で実行
      if exists
        then return $ User "Alice" ...
        else throwError err404       -- エラーは throwError で投げる
```

---

### 3. アーキテクチャの解像度向上：PolysemyによるEffects導入

実用的なアプリケーションでは `Handler` (IO) を直接使うのではなく、ビジネスロジックを抽象化するために `Polysemy` のようなEffect Systemと組み合わせるのがベストプラクティスです。

#### 3.1 PolysemyによるEffect定義
ビジネスロジック（例：DB操作）をEffectとして定義します,。

```haskell
-- Effect定義
data UserStore m a where
  InsertUser :: User -> UserStore m ()
  GetUser    :: Int  -> UserStore m (Maybe User)

makeSem ''UserStore -- ボイラープレートの自動生成
```

#### 3.2 解釈機 (Interpreter) の実装
Effectを `IO` (最終的に `Handler`) に変換する解釈機を実装します。

```haskell
runUserStoreIO :: Member (Embed IO) r => Sem (UserStore ': r) a -> Sem r a
runUserStoreIO = interpret $ \case
  InsertUser u -> embed $ print ("Inserting", u) -- 実際はDB処理
  GetUser i    -> embed $ return (Just $ User "Bob" ...)
```

#### 3.3 `hoistServer` による統合
Servantは `Handler` モナドを要求しますが、アプリケーションロジックは `Sem r` で記述したいです。これを繋ぐのが `hoistServer` です。

```haskell
-- アプリケーションロジックは Sem r で記述
userServerSem :: Member UserStore r => ServerT UserAPI (Sem r)
userServerSem = ...

-- 自然変換: Sem r a -> Handler a
nt :: Sem '[UserStore, Embed IO] a -> Handler a
nt sem =
  let
    -- PolysemyのEffectを剥がしてIOにする
    ioAction = runM . runUserStoreIO $ sem
  in
    liftIO ioAction

-- アプリケーションの起動
app :: Application
app = serve userAPI $
      hoistServer userAPI nt userServerSem -- Sem r から Handler へ変換
```

これにより、テスト時はオンメモリの `UserStore` 解釈機に差し替えるといった柔軟性が生まれます。

---

### 4. クライアント生成の解像度向上

Servantの強力な点は、サーバーの実装からクライアント関数を「導出」できる点です。

#### 4.1 Haskellクライアント
`client` 関数は、API型を受け取り、`ClientM` モナド内で動作する関数群を返します。

```haskell
-- クライアント関数の生成
getAll :<|> postOne :<|> getOne :<|> _ = client (Proxy :: Proxy UserAPI)

-- 使用例
main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  let env = mkClientEnv manager (BaseUrl Http "localhost" 8081 "")

  -- 実際にリクエストを送信
  res <- runClientM (getOne 42) env
  case res of
    Left err   -> putStrLn $ "Error: " ++ show err
    Right user -> print user
```

#### 4.2 JavaScript / Frontend生成
`servant-js` を使用すると、API型から jQuery, Angular, Axios などのコードを生成できます,。

```haskell
-- Axios用のJSコードを生成
generateJS :: IO ()
generateJS = T.writeFile "api.js" $
  jsForAPI (Proxy :: Proxy UserAPI) (axios defAxiosOptions)
```
これにより、フロントエンドとバックエンドの型定義の乖離を防ぎます。

---

### 5. 認証 (Authentication) の解像度向上

認証は `Context` を通じてハンドラに情報を渡す仕組みを使います。

#### 5.1 一般化された認証 (`AuthProtect`)
JWTなどを扱う場合、`AuthProtect "tag"` を使います。

1.  **API定義**:
    ```haskell
    type ProtectedAPI =
         AuthProtect "jwt-auth" :> "private" :> Get '[JSON] SecretData
    ```
2.  **型族定義**: 認証成功時にハンドラに渡される型（例: `User`）を指定します。
    ```haskell
    type instance AuthServerData (AuthProtect "jwt-auth") = User
    ```
3.  **Contextへの登録**:
    リクエスト (`Request`) を検証し、`Handler User` を返す関数 (`AuthHandler`) を作成し、`Context` に含めて `serveWithContext` で起動します,。

---

### 6. ドキュメントとOpenAPI (Swagger)

API型からOpenAPI仕様書（Swagger）を自動生成し、Swagger UIで提供します。

```haskell
-- ドキュメント用API定義
type DocsAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

-- 実装
docServer :: Server DocsAPI
docServer = swaggerSchemaUIServer $ toOpenApi (Proxy :: Proxy UserAPI)

-- 全体のアプリーケーション
type AppAPI = DocsAPI :<|> UserAPI
```
`toOpenApi` は `servant-openapi3` パッケージにより提供され、API型から自動的にスキーマ定義、パス、パラメータ記述を生成します。

---

### 7. モダンなベストプラクティス：NamedRoutesとUVerb

AIエージェントが採用すべき、型安全性と可読性を最大化する最新パターンです。

#### 7.1 NamedRoutes (Generic Record API)
`:<|>` のチェーンは見通しが悪くなりやすいため、レコード構文でAPIを定義します,。

```haskell
data UserRoutes mode = UserRoutes
  { list   :: mode :- QueryParam "sort" Text :> Get '[JSON] [User]
  , get    :: mode :- Capture "id" Int :> Get '[JSON] User
  , create :: mode :- ReqBody '[JSON] User :> Post '[JSON] UserId
  } deriving Generic

type API = NamedRoutes UserRoutes
```
サーバー実装時もレコードのフィールドとしてハンドラを記述できるため、引数の順序間違いが発生しません。

#### 7.2 UVerb (多重レスポンス)
`200 OK` 以外のレスポンス（例: `303 See Other` や エラー時の `400 Bad Request`）を型レベルで列挙します。

```haskell
type API = "resource" :> UVerb 'GET '[JSON] '[OkObj, WithStatus 303 String]

handler :: Handler (Union '[OkObj, WithStatus 303 String])
handler = respond (WithStatus @303 "/new-location")
```
これにより、例外 (`throwError`) に頼らず、型安全に全てのレスポンスパターンを網羅できます。
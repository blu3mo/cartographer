---
description: Canonical Test-Driven Development (TDD) in a Nutshell(after Kent Beck's “Canon TDD”)
---

 - 新機能に必要なすべての振る舞い（正常系、境界値、エラーパス、回帰）をブレインストーミングします。「どう実装するか（how）」ではなく「何が観測できるか（observable behavior）」に集中してください。Red - Pick exactly one item and turn it into a real, failing, automated test - リストから1つだけ選び、コンパイルが通り、かつアサーションが失敗する（または undefined で落ちる）テストを書きます。Haskellでは、型シグネチャを定義し、実装を undefined にすることで「Red」を開始するのが一般的です。Green - Change production code just enough to make all tests pass. - テストを通すためだけの「罪深い」コード（定数返しや、不完全なパターンマッチ）を書きます。「正しく作る」ことと「動くようにする」ことを明確に分離してください。Refactor (optional, but highly recommended). - テストが緑の状態で、内部設計を改善します。重複の排除、関数合成（.）やポイントフリースタイルの適用、適切な型への抽象化を行います。振る舞いは一切変えてはいけません。Loop. - リストから完了した項目を消し、次を選んで繰り返します。Outcome: 以前動作していたものは引き続き動作し、新しい振る舞いが機能し、コードは次の変更を受け入れる準備ができています。これら全てに客観的な自信を持てます。Common Mis-steps to Avoid (Haskell Context)MisunderstandingWhy it hurtsWriting all tests up front設計が固定化され、後の洞察による変更コストが増大します。Haskellでは型設計がテストの一部となるため、特に危険です。Removing assertionsundefined を消してコンパイルを通すだけでは不十分です。値の正当性を検証してください。Mixing refactor with green振る舞いの変更か、構造の変更か、失敗の原因が追えなくなります。Abstracting too early早すぎる型クラス化（Typeclass）やモナド化は、不要な複雑さを招きます。重複が3回現れるまで待ちましょう。Worked Example in Functional-Style Haskell不変配列（リスト）の平均値を算出する average 関数を実装します。Haskellの「純粋関数」と「型安全性」を活かし、実行時エラー（ゼロ除算など）を防ぐ設計にします。0 - Test List- returns Nothing when the list is empty              (safety/edge)
- returns Just value when length == 1                 (degenerate)
- returns arithmetic mean of three values             (happy path)
- property: result is always between min and max      (QuickCheck)
1 - RED最も単純なケース：空リスト → Nothing。Haskellでは 0.0 を返すのは安全ではない（意味がない）ため、Maybe を使います。-- test/AverageSpec.hs
import Test.Hspec
import Average (average)

main :: IO ()
main = hspec $ do
  describe "average" $ do
    it "returns Nothing when the list is empty" $ do
      average [] `shouldBe` Nothing
-- src/Average.hs
module Average where

-- 型シグネチャだけ定義し、実装は未定義（Red）
average :: [Int] -> Maybe Double
average = undefined
Compile → Run → Test fails (Prelude.undefined). 良し、Redバーです。2 - GREENこのテストだけを通す最小限のコードを書きます。-- src/Average.hs
module Average where

average :: [Int] -> Maybe Double
average _ = Nothing
Run tests → Green bar.3 - REFACTOR特になし。4 - LOOP次のテスト：length == 1。RED    it "returns the single value when length == 1" $ do
      average [42] `shouldBe` Just 42.0
失敗します（Nothing が返ってくるため）。GREEN - minimal extensionaverage :: [Int] -> Maybe Double
average [] = Nothing
average (x:_) = Just (fromIntegral x) -- 罪深い実装だが、テストは通る
すべてのテストが通ります。REFACTOR次のテスト（3つの値）を見越して、計算ロジックを実装します。ここで「一般化」を行います。average :: [Int] -> Maybe Double
average [] = Nothing
average xs = Just $ fromIntegral (sum xs) / fromIntegral (length xs)
純粋関数であり、副作用はなく、可変変数もありません。Haskellのイディオム通りです。テスト実行 → Green。5 - Continue Until Done (The Haskell Superpower)「3つの値の平均」テストを追加しても通りますが、Haskell使いならここで Property-based Testing (QuickCheck) を導入し、あらゆるケースを網羅します。-- test/AverageSpec.hs に追加
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

-- ... inside hspec block
    prop "always implies the result is >= minimum and <= maximum" $ \xs ->
      not (null xs) ==>
        let (Just avg) = average xs
        in avg >= fromIntegral (minimum xs) && avg <= fromIntegral (maximum xs)
これにより、手動でテストケースを追加するのではなく、自動生成された数百のリストで不変条件（invariant）を検証します。Putting It Together振る舞いのリストを作る。マイクロサイクルを回す: Red (undefined/fail) → Green (minimal pattern match) → Refactor.型システムを味方につける: Maybe や Either を使い、不正な状態を型レベルで排除する。プロパティで検証する: 個別の事例テストが終わったら、QuickCheckで「性質」をテストし、実装の堅牢性を確保する。このように、HaskellにおけるTDDは、単にテストを先に書くだけでなく、「型による設計」と「プロパティによる検証」をフィードバックループに組み込むエンジンとなります。
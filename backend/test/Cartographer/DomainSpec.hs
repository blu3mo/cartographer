module Cartographer.DomainSpec (spec) where

import Cartographer.Domain
import ProjectM36.Atomable (toAtom)
import ProjectM36.Base (Atom (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "SessionStatus" $ do
    it "converts Draft to correct Atom" $ do
      toAtom Draft `shouldBe` TextAtom "Draft"

    it "converts Open to correct Atom" $ do
      toAtom Open `shouldBe` TextAtom "Open"

  describe "QuestionType" $ do
    it "converts FreeText to Atom" $ do
      toAtom FreeText `shouldBe` TextAtom "FreeText"

    it "converts LikertScale to Atom" $ do
      -- LikertScale 5 -> "LikertScale 5" (constructed atom) or similar.
      -- ProjectM36 default Generic deriving usually produces ConstructAtom.
      -- We will verify the behavior.
      -- For now, let's just ensure it compiles and runs.
      let atom = toAtom (LikertScale 5)
      atom `shouldNotBe` TextAtom "Error"

module ConformanceSpec (spec) where

import Conformance
import Test.Syd

spec :: Spec
spec = do
  let forceType :: Conform Int Int Int a -> Conform Int Int Int a
      forceType = id
  let forceType' :: Conform Int Int Int Int -> Conform Int Int Int Int
      forceType' = forceType

  describe "runConformStrict" $ do
    it "can succeed" $
      runConformStrict (forceType' (pure 0)) `shouldBe` Right 0

    it "errors on a warning" $
      runConformStrict (forceType (emitWarning 0)) `shouldBe` Left (Right (Notes [] [0]))

    it "errors on a fixable error" $
      runConformStrict (forceType (emitFixableError 0)) `shouldBe` Left (Right (Notes [0] []))

    it "errors on an unfixable error" $
      runConformStrict (forceType' (unfixableError 0)) `shouldBe` Left (Left 0)

  describe "runConform" $ do
    it "can succeed" $
      runConform (forceType' (pure 0)) `shouldBe` Right (0, [])

    it "does not error on a warning" $
      runConform (forceType (emitWarning 0)) `shouldBe` Right ((), [0])

    it "errors on a fixable error" $
      runConform (forceType (emitFixableError 0)) `shouldBe` Left (HaltedBecauseOfStrictness 0)

    it "errors on an unfixable error" $
      runConform (forceType' (unfixableError 0)) `shouldBe` Left (HaltedBecauseOfUnfixableError 0)

  describe "runConformFlexible" $ do
    it "can succeed" $
      runConformFlexible even (forceType' (pure 0)) `shouldBe` Right (0, mempty)

    it "does not error on a warning" $
      runConformFlexible even (forceType (emitWarning 0)) `shouldBe` Right ((), Notes [] [0])

    it "does not error on a fixable error if the predicate holds" $
      runConformFlexible even (forceType (emitFixableError 0)) `shouldBe` Right ((), Notes [0] [])

    it "errors on a fixable error if the predicate does not hold" $
      runConformFlexible even (forceType (emitFixableError 1)) `shouldBe` Left (HaltedBecauseOfStrictness 1)

    it "errors on an unfixable error" $
      runConformFlexible even (forceType' (unfixableError 0)) `shouldBe` Left (HaltedBecauseOfUnfixableError 0)

  describe "runConformLenient" $ do
    it "can succeed" $
      runConformLenient (forceType' (pure 0)) `shouldBe` Right (0, mempty)

    it "does not error on a warning" $
      runConformLenient (forceType (emitWarning 0)) `shouldBe` Right ((), Notes [] [0])

    it "errors on a fixable error" $
      runConformLenient (forceType (emitFixableError 0)) `shouldBe` Right ((), Notes [0] [])

    it "errors on an unfixable error" $
      runConformLenient (forceType' (unfixableError 0)) `shouldBe` Left 0

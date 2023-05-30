{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.BinarySpec where

import ICal.PropertyType
import ICal.PropertyType.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Binary" $ do
    genValidSpec @Binary
    propertyTypeSpec @Binary

  describe "parseBinary" $
    it "roundtrips with renderBinary" $
      forAllValid $ \b -> do
        case parseBinary (renderBinary b) of
          Left err -> expectationFailure $ show err
          Right actual -> actual `shouldBe` b

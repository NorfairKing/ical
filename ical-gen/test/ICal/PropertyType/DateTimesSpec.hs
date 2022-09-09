{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.DateTimesSpec where

import ICal.PropertyType.Class
import ICal.PropertyType.DateTime
import ICal.PropertyType.DateTimes
import ICal.PropertyType.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "DateTimes" $ do
    genValidSpec @DateTimes
    propertyTypeSpec @DateTimes

  describe "dateTimeP" $ do
    it "can parse any single datetime as a datetimes set of one element" $
      forAllValid $ \dateTime ->
        let clv = propertyTypeB (dateTime :: DateTime)
         in case propertyTypeP clv :: Either String DateTimes of
              Left err -> expectationFailure err
              Right _ -> pure ()

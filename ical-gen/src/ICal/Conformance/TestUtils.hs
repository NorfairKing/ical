module ICal.Conformance.TestUtils where

import Control.Monad.IO.Class
import ICal.Conformance
import Test.Syd

shouldConformStrict :: (Show ue, Show fe, Show w) => Conform ue fe w a -> IO a
shouldConformStrict = assertStrictResultSucceeded . runConformStrict

shouldConformTStrict :: (Show ue, Show fe, Show w, MonadIO m) => ConformT ue fe w m a -> m a
shouldConformTStrict func = do
  errOrErrOrResult <- runConformTStrict func
  liftIO $ assertStrictResultSucceeded errOrErrOrResult

assertStrictResultSucceeded :: (Show ue, Show fe, Show w) => Either (Either ue (Notes fe w)) a -> IO a
assertStrictResultSucceeded errOrErrOrResult =
  case errOrErrOrResult of
    Left e -> expectationFailure $ show e
    Right a -> pure a

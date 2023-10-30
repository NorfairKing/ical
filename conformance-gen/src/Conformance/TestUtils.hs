module Conformance.TestUtils where

import Conformance
import Control.Monad.IO.Class
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

shouldConform :: (Show ue, Show fe) => Conform ue fe w a -> IO a
shouldConform func =
  case runConform func of
    Left hr ->
      expectationFailure $
        unwords
          [ "runConform failed",
            ppShow hr
          ]
    Right (a, _) -> pure a

shouldConformT :: (Show ue, Show fe, MonadIO m) => ConformT ue fe w m a -> m a
shouldConformT func = do
  errOrErrOrResult <- runConformT func
  case errOrErrOrResult of
    Left hr ->
      liftIO $
        expectationFailure $
          unwords
            [ "runConformT failed",
              ppShow hr
            ]
    Right (a, _) -> pure a

shouldConformLenient :: Show ue => Conform ue fe w a -> IO a
shouldConformLenient = assertLenientResultSucceeded . runConformLenient

shouldConformTLenient :: (Show ue, MonadIO m) => ConformT ue fe w m a -> m a
shouldConformTLenient func = do
  errOrErrOrResult <- runConformTLenient func
  liftIO $ assertLenientResultSucceeded errOrErrOrResult

assertLenientResultSucceeded :: Show ue => Either ue (a, Notes fe w) -> IO a
assertLenientResultSucceeded errOrErrOrResult =
  case errOrErrOrResult of
    Left e -> expectationFailure $ show e
    Right (a, _) -> pure a

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ICal.Parameter.Class where

import Control.Exception
import Control.Monad
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Proxy
import Data.Text (Text)
import Data.Validity.Time ()
import Data.Void
import ICal.Conformance
import ICal.ContentLine
import Text.Megaparsec

deriving instance Ord s => Ord (PosState s)

deriving instance (Ord s, Ord (Token s), Ord e) => Ord (ParseError s e)

deriving instance (Ord s, Ord (Token s), Ord e) => Ord (ParseErrorBundle s e)

data ParameterParseError
  = UnquotedParameterFound !(CI Text)
  | UnknownEncoding !ParamValue
  | UnknownRecurrenceIdentifierRange !ParamValue -- TODO we can turn this into a fixable error by guessing the default value.
  | UnknownRSVPExpectation !ParamValue -- TODO we can turn this into a fixable error by guessing the default value.
  | UnknownAlarmTriggerRelationship !ParamValue -- TODO we can turn this into a fixable error by guessing the default value.
  | InvalidCalAddress !Text
  deriving (Show, Eq, Ord)

instance Exception ParameterParseError where
  displayException = \case
    UnquotedParameterFound value ->
      unlines
        [ "An unquoted parameter value found where a quoted one was expected.",
          "value:",
          show value
        ]
    UnknownEncoding pv ->
      unlines
        [ "Unknown ENCODING Value:",
          show pv
        ]
    UnknownRecurrenceIdentifierRange pv ->
      unlines
        [ "Unknown RANGE Value:",
          show pv
        ]
    UnknownRSVPExpectation pv ->
      unlines
        [ "Unknown RSVP Value:",
          show pv
        ]
    UnknownAlarmTriggerRelationship pv ->
      unlines
        [ "Unknown RELATED Value:",
          show pv
        ]
    InvalidCalAddress t ->
      unlines
        [ "Invalid cal-address in a parameter",
          show t
        ]

data ParameterParseFixableError
  = MultipleParametersfound !(NonEmpty ParamValue)
  deriving (Show, Eq, Ord)

instance Exception ParameterParseFixableError where
  displayException = \case
    MultipleParametersfound values ->
      unlines
        [ "Multiple parameter values found where one was expected.",
          "values:",
          show values
        ]

-- | Parameters
--
-- === Laws
--
-- * The 'NonEmpty ParamValue' that is built is valid:
--
-- >>> forAllValid $ \parameter -> isValid (parameterB parameter)
--
-- * Anything parsed is valid:
--
-- >>> forAllValid $ \paramValue -> isValid (parameterP paramValue)
--
-- * The parameter roundtrips through 'ContentLineValue'.
--
-- >>> forAllValid $ \parameter -> runConformStrict (parameterP (parameterB parameter)) == Right parameter
class IsParameter param where
  -- Name of the parameter
  parameterName :: Proxy param -> ParamName

  -- | Parser for the parameter
  parameterP :: ParamValue -> Conform ParameterParseError ParameterParseFixableError Void param

  -- | Builder for the parameter
  parameterB :: param -> ParamValue

lookupParam ::
  forall param.
  IsParameter param =>
  Map ParamName (NonEmpty ParamValue) ->
  Maybe (Conform ParameterParseError ParameterParseFixableError Void param)
lookupParam m = do
  let name = parameterName (Proxy :: Proxy param)
  ne@(paramValue :| rest) <- M.lookup name m
  pure $ do
    when (not (null rest)) $ emitFixableError $ MultipleParametersfound ne
    parameterP paramValue

optionalParam ::
  forall param.
  IsParameter param =>
  Map ParamName (NonEmpty ParamValue) ->
  Conform ParameterParseError ParameterParseFixableError Void (Maybe param)
optionalParam m = do
  let name = parameterName (Proxy :: Proxy param)
  forM (M.lookup name m) $ \ne@(paramValue :| rest) -> do
    when (not (null rest)) $ emitFixableError $ MultipleParametersfound ne
    parameterP paramValue

listParam ::
  forall param.
  IsParameter param =>
  Map ParamName (NonEmpty ParamValue) ->
  Conform ParameterParseError ParameterParseFixableError Void [param]
listParam m = do
  let name = parameterName (Proxy :: Proxy param)
  let paramValues = maybe [] NE.toList (M.lookup name m)
  mapM parameterP paramValues

insertParam :: forall param. IsParameter param => param -> ContentLineValue -> ContentLineValue
insertParam param clv =
  clv
    { contentLineValueParams =
        M.insert (parameterName (Proxy :: Proxy param)) (parameterB param :| []) (contentLineValueParams clv)
    }

insertMParam :: forall param. IsParameter param => Maybe param -> ContentLineValue -> ContentLineValue
insertMParam = maybe id insertParam

insertParamNE :: forall param. IsParameter param => NonEmpty param -> ContentLineValue -> ContentLineValue
insertParamNE params clv =
  clv
    { contentLineValueParams =
        M.insert (parameterName (Proxy :: Proxy param)) (NE.map parameterB params) (contentLineValueParams clv)
    }

insertParamList :: forall param. IsParameter param => [param] -> ContentLineValue -> ContentLineValue
insertParamList params clv =
  case NE.nonEmpty params of
    Nothing -> clv
    Just paramsNE -> insertParamNE paramsNE clv

insertParamWithDefault ::
  forall param.
  (Eq param, IsParameter param) =>
  param ->
  param ->
  ContentLineValue ->
  ContentLineValue
insertParamWithDefault defaultParam param clv =
  if param == defaultParam
    then clv
    else insertParam param clv

-- TODO figure out if this text should be case-insensitive
anySingleParamP ::
  (CI Text -> Conform ParameterParseError ParameterParseFixableError void a) ->
  ParamValue ->
  Conform ParameterParseError ParameterParseFixableError void a
anySingleParamP func = \case
  UnquotedParam c -> func c
  QuotedParam t -> func (CI.mk t)

quotedParamP ::
  (Text -> Conform ParameterParseError ParameterParseFixableError void a) ->
  ParamValue ->
  Conform ParameterParseError ParameterParseFixableError void a
quotedParamP func = \case
  QuotedParam t -> func t
  UnquotedParam ci -> unfixableError $ UnquotedParameterFound ci -- TODO turn this into a fixable error.

quotedParamB :: (a -> Text) -> a -> ParamValue
quotedParamB func = QuotedParam . func

-- TODO figure out if this text should be case-insensitive
anySingleParamB :: (a -> CI Text) -> a -> ParamValue
anySingleParamB func a =
  let ci = func a
      o = CI.original ci
   in if haveToQuoteText o
        then QuotedParam o
        else UnquotedParam ci

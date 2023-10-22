{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ICal.Parameter.Class where

import Control.Exception
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Proxy
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
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
  = ParameterNotFound !ParamName !(Map ParamName (NonEmpty ParamValue))
  | MultipleParametersfound !(NonEmpty ParamValue)
  | UnquotedParameterFound !(CI Text)
  | UnknownEncoding !ParamValue
  | UnknownRecurrenceIdentifierRange !ParamValue -- TODO we can turn this into a fixable error by guessing the default value.
  | UnknownRSVPExpectation !ParamValue -- TODO we can turn this into a fixable error by guessing the default value.
  | UnknownAlarmTriggerRelationship !ParamValue -- TODO we can turn this into a fixable error by guessing the default value.
  | InvalidCalAddress !Text
  deriving (Show, Eq, Ord)

instance Exception ParameterParseError where
  displayException = \case
    ParameterNotFound name m ->
      unlines
        [ "Parameter not found: " <> show name,
          "while looking through these parameters:",
          show m
        ]
    MultipleParametersfound values ->
      unlines
        [ "Multiple parameter values found where one was expected.",
          "values:",
          show values
        ]
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
-- >>> forAllValid $ \paramValues -> isValid (parameterP paramValues)
--
-- * The parameter roundtrips through 'ContentLineValue'.
--
-- >>> forAllValid $ \parameter -> parameterP (parameterB parameter) == Right parameter
class IsParameter param where
  -- Name of the parameter
  parameterName :: Proxy param -> ParamName

  -- | Parser for the parameter
  parameterP :: NonEmpty ParamValue -> Conform ParameterParseError Void Void param

  -- | Builder for the parameter
  parameterB :: param -> NonEmpty ParamValue

lookupParam :: forall param. IsParameter param => Map ParamName (NonEmpty ParamValue) -> Maybe (Conform ParameterParseError Void Void param)
lookupParam m = do
  let name = parameterName (Proxy :: Proxy param)
  pvs <- M.lookup name m
  pure $ parameterP pvs

optionalParam :: forall param. IsParameter param => Map ParamName (NonEmpty ParamValue) -> Conform ParameterParseError Void Void (Maybe param)
optionalParam m =
  let name = parameterName (Proxy :: Proxy param)
   in mapM parameterP (M.lookup name m)

listParam :: forall param. IsParameter param => Map ParamName (NonEmpty ParamValue) -> Conform ParameterParseError Void Void [param]
listParam m =
  let name = parameterName (Proxy :: Proxy param)
   in mapM (parameterP . (:| [])) (maybe [] NE.toList (M.lookup name m))

optionalParamSet ::
  forall param.
  (Ord param, IsParameter param) =>
  Map ParamName (NonEmpty ParamValue) ->
  Conform ParameterParseError Void Void (Maybe (Set param))
optionalParamSet m =
  let name = parameterName (Proxy :: Proxy param)
   in mapM
        (fmap S.fromList . mapM (parameterP . (:| [])) . NE.toList)
        (M.lookup name m)

requireParam :: forall param. IsParameter param => Map ParamName (NonEmpty ParamValue) -> Conform ParameterParseError Void Void param
requireParam m = case lookupParam m of
  Just errOrResult -> errOrResult
  Nothing -> unfixableError $ ParameterNotFound (parameterName (Proxy :: Proxy param)) m

paramMap :: forall param. IsParameter param => param -> Map ParamName (NonEmpty ParamValue)
paramMap param = M.singleton (parameterName (Proxy :: Proxy param)) (parameterB param)

setParamMap :: forall param. IsParameter param => Set param -> Map ParamName (NonEmpty ParamValue)
setParamMap params = case NE.nonEmpty (map parameterB (S.toList params)) of
  Nothing -> M.empty
  Just ne -> M.singleton (parameterName (Proxy :: Proxy param)) (sconcat ne)

insertParam :: forall param. IsParameter param => param -> ContentLineValue -> ContentLineValue
insertParam param clv =
  clv
    { contentLineValueParams =
        M.insert (parameterName (Proxy :: Proxy param)) (parameterB param) (contentLineValueParams clv)
    }

insertMParam :: forall param. IsParameter param => Maybe param -> ContentLineValue -> ContentLineValue
insertMParam = maybe id insertParam

insertParamList :: forall param. IsParameter param => [param] -> ContentLineValue -> ContentLineValue
insertParamList params clv =
  case NE.nonEmpty params of
    Nothing -> clv
    Just paramsNE ->
      clv
        { contentLineValueParams =
            M.insert (parameterName (Proxy :: Proxy param)) (sconcat (NE.map parameterB paramsNE)) (contentLineValueParams clv)
        }

insertParamWithDefault :: forall param. (Eq param, IsParameter param) => param -> param -> ContentLineValue -> ContentLineValue
insertParamWithDefault defaultParam param clv =
  if param == defaultParam
    then clv
    else insertParam param clv

singleParamP :: (ParamValue -> Conform ParameterParseError void void' a) -> NonEmpty ParamValue -> Conform ParameterParseError void void' a
singleParamP func = \case
  value :| [] -> func value
  ne -> unfixableError $ MultipleParametersfound ne

-- TODO figure out if this text should be case-insensitive
anySingleParamP ::
  (CI Text -> Conform ParameterParseError void void' a) ->
  NonEmpty ParamValue ->
  Conform ParameterParseError void void' a
anySingleParamP func = singleParamP $ \case
  UnquotedParam c -> func c
  QuotedParam t -> func (CI.mk t)

singleQuotedParamP ::
  (Text -> Conform ParameterParseError void void' a) ->
  NonEmpty ParamValue ->
  Conform ParameterParseError void void' a
singleQuotedParamP func = \case
  value :| [] -> case value of
    QuotedParam t -> func t
    UnquotedParam ci -> unfixableError $ UnquotedParameterFound ci -- TODO turn this into a fixable error.
  ne -> unfixableError $ MultipleParametersfound ne

singleQuotedParamB :: (a -> Text) -> a -> NonEmpty ParamValue
singleQuotedParamB func = singleParamB $ QuotedParam . func

singleParamB :: (a -> ParamValue) -> a -> NonEmpty ParamValue
singleParamB func = (:| []) . func

-- TODO figure out if this text should be case-insensitive
anySingleParamB :: (a -> CI Text) -> a -> NonEmpty ParamValue
anySingleParamB func = singleParamB $ \a ->
  let ci = func a
      o = CI.original ci
   in if haveToQuoteText o
        then QuotedParam o
        else UnquotedParam ci

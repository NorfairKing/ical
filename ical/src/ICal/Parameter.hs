{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Parameter where

import Control.DeepSeq
import Control.Monad
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Proxy
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text
import Data.Validity.Time ()
import GHC.Generics (Generic)
import ICal.ContentLine

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
  parameterP :: NonEmpty ParamValue -> Either String param

  -- | Builder for the parameter
  parameterB :: param -> NonEmpty ParamValue

lookupParam :: forall param. IsParameter param => Map ParamName (NonEmpty ParamValue) -> Maybe (Either String param)
lookupParam m = do
  let name = parameterName (Proxy :: Proxy param)
  pvs <- M.lookup name m
  pure $ parameterP pvs

optionalParam :: forall param. IsParameter param => Map ParamName (NonEmpty ParamValue) -> Either String (Maybe param)
optionalParam m =
  let name = parameterName (Proxy :: Proxy param)
   in mapM parameterP (M.lookup name m)

optionalParamSet ::
  forall param.
  (Ord param, IsParameter param) =>
  Map ParamName (NonEmpty ParamValue) ->
  Either String (Maybe (Set param))
optionalParamSet m =
  let name = parameterName (Proxy :: Proxy param)
   in mapM
        (fmap S.fromList . mapM (parameterP . (:| [])) . NE.toList)
        (M.lookup name m)

requireParam :: forall param. IsParameter param => Map ParamName (NonEmpty ParamValue) -> Either String param
requireParam m = case lookupParam m of
  Just errOrResult -> errOrResult
  Nothing ->
    Left $
      unlines
        [ "Parameter not found: " <> show (parameterName (Proxy :: Proxy param)),
          "while looking through these parameters:",
          show m
        ]

paramMap :: forall param. IsParameter param => param -> Map ParamName (NonEmpty ParamValue)
paramMap param = M.singleton (parameterName (Proxy :: Proxy param)) (parameterB param)

setParamMap :: forall param. IsParameter param => Set param -> Map ParamName (NonEmpty ParamValue)
setParamMap params = case NE.nonEmpty (map parameterB (S.toList params)) of
  Nothing -> M.empty
  Just ne -> M.singleton (parameterName (Proxy :: Proxy param)) (sconcat ne)

singleParamP :: (ParamValue -> Either String a) -> NonEmpty ParamValue -> Either String a
singleParamP func = \case
  value :| [] -> func value
  _ -> Left "Expected one parameter value, but got multiple."

anySingleParamP :: (Text -> Either String a) -> NonEmpty ParamValue -> Either String a
anySingleParamP func = singleParamP $ \case
  UnquotedParam c -> func (CI.foldedCase c)
  QuotedParam t -> func t

-- [section 3.2.19](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.19)
newtype TZIDParam = TZIDParam {unTZIDParam :: CI Text}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, IsString, Read)

instance Validity TZIDParam where
  validate p@(TZIDParam ci) =
    mconcat
      [ genericValidate p,
        decorateText (CI.original ci) validateSafeChar
      ]

instance NFData TZIDParam

instance IsParameter TZIDParam where
  parameterName Proxy = "TZID"
  parameterP = tzIDParamP
  parameterB = tzIDParamB

tzIDParamP :: NonEmpty ParamValue -> Either String TZIDParam
tzIDParamP = singleParamP $
  (prettyValidate <=<) $ \case
    UnquotedParam c -> Right $ TZIDParam {unTZIDParam = c}
    p -> Left $ "Expected TZIDParam to be unquoted, but was quoted: " <> show p

tzIDParamB :: TZIDParam -> NonEmpty ParamValue
tzIDParamB = (:| []) . UnquotedParam . unTZIDParam

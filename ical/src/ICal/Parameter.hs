{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Parameter where

import Data.CaseInsensitive (CI)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Proxy
import Data.String
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
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

singleParamP :: (ParamValue -> Either String TZIDParam) -> NonEmpty ParamValue -> Either String TZIDParam
singleParamP func = \case
  value :| [] -> func value
  _ -> Left "Expected one parameter value, but got multiple."

-- [section 3.2.19](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.19)
newtype TZIDParam = TZIDParam {unTZIDParam :: CI Text}
  deriving stock (Eq, Generic)
  deriving newtype (Show, IsString, Read)

instance Validity TZIDParam

instance IsParameter TZIDParam where
  parameterName Proxy = "TZID"
  parameterP = tzIDParamP
  parameterB = tzIDParamB

tzIDParamP :: NonEmpty ParamValue -> Either String TZIDParam
tzIDParamP = singleParamP $ \case
  UnquotedParam c -> Right $ TZIDParam {unTZIDParam = c}
  p -> Left $ "Expected TZIDParam to be unquoted, but was quoted: " <> show p

tzIDParamB :: TZIDParam -> NonEmpty ParamValue
tzIDParamB = (:| []) . UnquotedParam . unTZIDParam

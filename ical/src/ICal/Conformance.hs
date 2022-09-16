{-# LANGUAGE LambdaCase #-}

module ICal.Conformance where

data Conform w e a
  = Conforming a
  | -- |
    -- for "SHOULD"s and "SHOULD NOT"s that were not met
    WithWarnings [w] a
  | -- |
    -- for "MUST"s and "MUST NOT"s that were not met
    Errors [e]

instance Functor (Conform w e) where
  fmap f = \case
    Conforming a -> Conforming (f a)
    WithWarnings ws a -> WithWarnings ws (f a)
    Errors es -> Errors es

instance Applicative (Conform w e) where
  pure = Conforming
  (<*>) ff fa = case (ff, fa) of
    (Conforming f, Conforming a) -> Conforming (f a)
    (Conforming f, WithWarnings ws a) -> WithWarnings ws (f a)
    (WithWarnings ws f, Conforming a) -> WithWarnings ws (f a)
    (WithWarnings ws1 f, WithWarnings ws2 a) -> WithWarnings (ws1 ++ ws2) (f a)
    (Errors es1, Errors es2) -> Errors (es1 ++ es2)
    (_, Errors es2) -> Errors es2
    (Errors es1, _) -> Errors es1

instance Monad (Conform w e) where
  (>>=) m f = case m of
    Conforming a -> f a
    WithWarnings ws1 a -> case f a of
      Conforming b -> Conforming b
      WithWarnings ws2 b -> WithWarnings (ws1 ++ ws2) b
      Errors es -> Errors es
    Errors es -> Errors es

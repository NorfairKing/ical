{-# LANGUAGE LambdaCase #-}

module ICal.Conformance where

data Conform w fe ue a
  = Conforming a
  | -- |
    -- For "SHOULD"s and "SHOULD NOT"s that were not met
    WithWarnings [w] a
  | -- |
    -- For "MUST"s and "MUST NOT"s that were not met
    WithFixableErrors
      [w]
      [fe]
      a
  | -- |
    -- For unfixable errors.
    Errors [ue]

instance Functor (Conform w fe ue) where
  fmap f = \case
    Conforming a -> Conforming (f a)
    WithWarnings ws a -> WithWarnings ws (f a)
    WithFixableErrors ws fes a -> WithFixableErrors ws fes (f a)
    Errors es -> Errors es

instance Applicative (Conform w fe ue) where
  pure = Conforming
  (<*>) ff fa = case (ff, fa) of
    (Conforming f, Conforming a) -> Conforming (f a)
    (Conforming f, WithWarnings ws a) -> WithWarnings ws (f a)
    (Conforming f, WithFixableErrors ws fes a) -> WithFixableErrors ws fes (f a)
    (WithWarnings ws f, Conforming a) -> WithWarnings ws (f a)
    (WithWarnings ws1 f, WithWarnings ws2 a) -> WithWarnings (ws1 ++ ws2) (f a)
    (WithWarnings ws1 f, WithFixableErrors ws2 fes a) -> WithFixableErrors (ws1 ++ ws2) fes (f a)
    (WithFixableErrors ws fes f, Conforming a) -> WithFixableErrors ws fes (f a)
    (WithFixableErrors ws1 fes f, WithWarnings ws2 a) -> WithFixableErrors (ws1 ++ ws2) fes (f a)
    (WithFixableErrors ws1 fes1 f, WithFixableErrors ws2 fes2 a) -> WithFixableErrors (ws1 ++ ws2) (fes1 ++ fes2) (f a)
    (Errors es1, Errors es2) -> Errors (es1 ++ es2)
    (_, Errors es2) -> Errors es2
    (Errors es1, _) -> Errors es1

instance Monad (Conform w fe ue) where
  (>>=) m f = case m of
    Conforming a -> f a
    WithWarnings ws1 a -> case f a of
      Conforming b -> Conforming b
      WithWarnings ws2 b -> WithWarnings (ws1 ++ ws2) b
      WithFixableErrors ws2 fes b -> WithFixableErrors (ws1 ++ ws2) fes b
      Errors es -> Errors es
    WithFixableErrors ws1 fes1 a -> case f a of
      Conforming b -> Conforming b
      WithWarnings ws2 b -> WithFixableErrors (ws1 ++ ws2) fes1 b
      WithFixableErrors ws2 fes2 b -> WithFixableErrors (ws1 ++ ws2) (fes1 ++ fes2) b
      Errors es -> Errors es
    Errors es -> Errors es

emitWarning :: w -> Conform w fe ue ()
emitWarning w = WithWarnings [w] ()

emitFixableError :: fe -> Conform w fe ue ()
emitFixableError fe = WithFixableErrors [] [fe] ()

unfixableError :: ue -> Conform w fe ue a
unfixableError ue = Errors [ue]

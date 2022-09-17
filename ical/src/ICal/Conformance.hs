{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ICal.Conformance where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer.Strict

-- | A conforming monad transformer to compute a result according to a spec.
--
-- RFC 2119 describes these terms:
--
-- 1. MUST and MUST NOT:
--    These describe absolute requirements or absolute prohibitions.
--    However, some implementations still do not adhere to these.
--    Some of those situations are fixable, and some are not.
--
--    If the situation is fixable, we can either error out (a strict implementation) or apply the fix.
--    The @fe@ parameter represents fixable errors, which can either be emitted as warnings, or errored on.
--    A predicate @(fe -> Bool)@ decides whether to fix the error. (The predicate returns True.)
-- 2. SHOULD and SHOULD NOT:
--    These describe weaker requirements or prohibitions.
--    The @w@ parameter represents warnings to represent cases where requirements or prohibitions were violated.
newtype ConformT w fe ue m a = Conform
  { unConform ::
      ReaderT (fe -> Bool) (ExceptT (HaltReason fe ue) (WriterT (Notes w fe) m)) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (fe -> Bool),
      MonadError (HaltReason fe ue),
      MonadWriter (Notes w fe)
    )

data HaltReason fe ue
  = HaltedBecauseOfStrictness fe
  | HaltedBecauseOfUnfixableError ue

data Notes w fe = Notes
  { notesWarnings :: [w],
    notesFixableErrors :: [fe]
  }

instance Semigroup (Notes w fe) where
  (<>) (Notes ws1 fes1) (Notes ws2 fes2) =
    Notes
      { notesWarnings = ws1 ++ ws2,
        notesFixableErrors = fes1 ++ fes2
      }

instance Monoid (Notes w fe) where
  mempty = Notes [] []
  mappend = (<>)

runConformT ::
  (fe -> Bool) ->
  ConformT w fe ue m a ->
  m (Either (HaltReason fe ue) (Notes w fe, a))
runConformT = undefined

emitWarning :: Monad m => w -> ConformT w fe ue m ()
emitWarning w = tell (Notes [w] [])

emitFixableError :: Monad m => fe -> ConformT w fe ue m ()
emitFixableError fe = do
  predicate <- ask
  if predicate fe
    then tell (Notes [] [fe])
    else throwError (HaltedBecauseOfStrictness fe)

unfixableError :: Monad m => ue -> ConformT w fe ue m a
unfixableError ue = throwError (HaltedBecauseOfUnfixableError ue)

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ICal.Conformance where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.Functor.Identity

-- | A conforming monad transformer to compute a result according to a spec.
--
-- RFC 2119 describes these terms:
--
-- 1. MUST and MUST NOT:
--    These describe absolute requirements or absolute prohibitions.
--    However, some implementations still do not adhere to these.
--    Some of those situations are fixable, and some are not.
--
--    If the situation is fixable, we error with an error of type @ue@.
--
--    If the situation is fixable, we can either error out (a strict implementation) with an error of type @fe@ or apply the fix.
--    The @fe@ parameter represents fixable errors, which can either be emitted as warnings, or errored on.
--    A predicate @(fe -> Bool)@ decides whether to fix the error. (The predicate returns True if the fixable error is to be fixed.)
-- 2. SHOULD and SHOULD NOT:
--    These describe weaker requirements or prohibitions.
--    The @w@ parameter represents warnings to represent cases where requirements or prohibitions were violated.
newtype ConformT ue fe w m a = ConformT
  { unConformT ::
      ReaderT (fe -> Bool) (WriterT (Notes fe w) (ExceptT (HaltReason ue fe) m)) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (fe -> Bool),
      MonadError (HaltReason ue fe),
      MonadWriter (Notes fe w)
    )

data HaltReason ue fe
  = HaltedBecauseOfUnfixableError ue
  | HaltedBecauseOfStrictness fe

data Notes fe w = Notes
  { notesFixableErrors :: [fe],
    notesWarnings :: [w]
  }

instance Semigroup (Notes w fe) where
  (<>) (Notes fes1 ws1) (Notes fes2 ws2) =
    Notes
      { notesFixableErrors = fes1 ++ fes2,
        notesWarnings = ws1 ++ ws2
      }

instance Monoid (Notes w fe) where
  mempty = Notes [] []
  mappend = (<>)

runConformT ::
  (fe -> Bool) ->
  ConformT ue fe w m a ->
  m (Either (HaltReason ue fe) (a, Notes fe w))
runConformT predicate (ConformT func) = runExceptT (runWriterT (runReaderT func predicate))

type Conform ue fe w a = ConformT ue fe w Identity a

runConform :: (fe -> Bool) -> Conform ue fe w a -> Either (HaltReason ue fe) (a, Notes fe w)
runConform predicate = runIdentity . runConformT predicate

emitWarning :: Monad m => w -> ConformT ue fe w m ()
emitWarning w = tell (Notes [] [w])

emitFixableError :: Monad m => fe -> ConformT ue fe w m ()
emitFixableError fe = do
  predicate <- ask
  if predicate fe
    then tell (Notes [fe] [])
    else throwError (HaltedBecauseOfStrictness fe)

unfixableError :: Monad m => ue -> ConformT ue fe w m a
unfixableError ue = throwError (HaltedBecauseOfUnfixableError ue)

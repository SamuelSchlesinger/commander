{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{- |
Module: Control.Monad.Commander
Description: A monad for stateful, backtracking computations
Copyright: (c) Samuel Schlesinger 2020
License: MIT
Maintainer: sgschlesinger@gmail.com
Stability: experimental
Portability: POSIX, Windows
-}
module Control.Monad.Commander (
  -- ** The CommanderT Monad
  {- |
    The 'CommanderT' monad is stateful and has the ability to backtrack.
  -}
  CommanderT(Action, Defeat, Victory), runCommanderT, hoistToFunctor, hoistFromFunctor,
) where

import Control.Arrow (first)
import Control.Monad (ap, MonadPlus)
import Control.Monad.Trans (MonadTrans, lift, liftIO, MonadIO)
import Control.Applicative (Alternative(empty, (<|>)))

-- | A 'CommanderT' action is a metaphor for a military commander. At each
-- step, we have a new 'Action' to take, or we could have experienced
-- 'Defeat', or we can see 'Victory'. While a real life commander
-- worries about moving his troops around in order to achieve a victory in
-- battle, a 'CommanderT' worries about iteratively transforming a state 
-- to find some value.
--
-- In more practical terms, a term of type 'CommanderT' can be thought of
-- as a backtracking, stateful computation which can either result in
-- a result being produced, or nothing being produced. It is a
-- 'Monad' for any base 'Functor' you want to use as the effect inside of
-- the stateful computation, similarly to the free monad.
data CommanderT state f a
  = Action (state -> f (CommanderT state f a, state))
  | Defeat
  | Victory a
  deriving Functor

-- | We can run a 'CommanderT' on some state and see if it has
-- a successful campaign.
runCommanderT :: Monad m 
              => CommanderT state m a 
              -> state 
              -> m (Maybe a)
runCommanderT (Action action) state = action state >>= uncurry runCommanderT
runCommanderT Defeat _ = return Nothing
runCommanderT (Victory a) _ = return (Just a)

-- | We can go from a non-'Functor' to a 'Functor' inside of a 'CommanderT'
-- action. This does the transformation "top to bottom", as opposed to
-- 'hoistFromFunctor', which does it "bottom to top". If your natural
-- transformation is lessening, i.e. it trims branching structure, then you
-- probably want to use this function.
hoistToFunctor :: Functor g => (forall a. f a -> g a) -> CommanderT state f a -> CommanderT state g a
hoistToFunctor phi (Action action) = Action (fmap (fmap (first (hoistToFunctor phi))) $ fmap phi action)

-- | We can go from a 'Functor' to a non-'Functor' inside of a 'CommanderT'
-- action. This does the transformation "bottom to top", as opposed to
-- 'hoistToFunctor', which does it "top to bottom". If your natural
-- transformation is increasing, i.e. it adds branching structure, then you
-- probably want to use this function.
hoistFromFunctor :: Functor f => (forall a. f a -> g a) -> CommanderT state f a -> CommanderT state g a
hoistFromFunctor phi (Action action) = Action (fmap phi $ fmap (fmap (first (hoistFromFunctor phi))) action)

instance Functor f => Applicative (CommanderT state f) where
  (<*>) = ap
  pure = Victory

instance MonadTrans (CommanderT state) where
  lift ma = Action $ \state -> do
    a <- ma
    return (pure a, state)

instance MonadIO m => MonadIO (CommanderT state m) where
  liftIO ma = Action $ \state -> do
    a <- liftIO ma
    return (pure a, state)

instance Functor f => Monad (CommanderT state f) where
  Defeat >>= _ = Defeat
  Victory a >>= f = f a
  Action action >>= f = Action (fmap (\(a, s) -> (a >>= f, s)) . action)

instance Functor f => Alternative (CommanderT state f) where
  empty = Defeat 
  Defeat <|> a = a 
  v@(Victory _) <|> _ = v
  Action action <|> p = Action (fmap (\(a, s) -> (a <|> p, s)) . action)

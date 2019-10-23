module Control.Monad.Extra
  ( ifM
  , whenM
  ) where

import           Control.Monad (when)

-- | Monadic version if @if@
ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM condition x y = do
  conditionResult <- condition
  if conditionResult
    then x
    else y

-- | Monadic version of @when@
whenM :: (Monad m) => m Bool -> m () -> m ()
whenM condition action = do
  conditionResult <- condition
  when conditionResult action

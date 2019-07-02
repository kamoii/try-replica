module P
  ( module P
  , module Relude
  ) where

import Relude hiding (withState)

-- monad control flows

untilRight
  :: Monad m
  => v
  -> (v -> m (Either v r))
  -> m r
untilRight v f = do
  e <- f v
  case e of
    Left v' -> untilRight v' f
    Right r -> pure r

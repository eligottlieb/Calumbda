module Errors where

import Control.Monad.Trans.Free
import Control.Monad.Identity
import Control.Monad.Except

type ThrowsError e = Either e
type PartialT m = FreeT Identity m
type ThrowsPartial e = PartialT (ThrowsError e)

runPartialT :: PartialT m a -> m (FreeF Identity a (PartialT m a))
runPartialT = runFreeT

rewrap :: Monad m => PartialT m a -> PartialT m a
rewrap = Control.Monad.Trans.Free.wrap . Identity

unwrap :: Monad m => PartialT m a -> PartialT m a
unwrap m = FreeT $ do
  x <- runFreeT m
  case x of
    Pure a -> return $ Pure a
    Free (Identity w) -> runFreeT w

unwrapN :: Monad m => Integer -> PartialT m a -> PartialT m a
unwrapN n m = (iterate unwrap m) !! (fromIntegral n)

liftPartial :: Monad m => m a -> PartialT m a
liftPartial a = FreeT (a >>= return . Pure)

runThrowsPartial :: ThrowsPartial e a -> ThrowsError e (FreeF Identity a (ThrowsPartial e a))
runThrowsPartial = runFreeT

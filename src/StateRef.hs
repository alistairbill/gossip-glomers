module StateRef where

import Control.Lens
import Control.Lens.Internal.Zoom

newtype StateRefT s m a = StateRefT
  { unStateRefT :: ReaderT (IORef s) m a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

runStateRefT :: StateRefT s m a -> IORef s -> m a
runStateRefT (StateRefT action) = runReaderT action

instance (MonadIO m) => MonadState s (StateRefT s m) where
  get = StateRefT $ ReaderT $ liftIO . readIORef
  put x = StateRefT $ ReaderT $ \ref -> liftIO $ writeIORef ref x

type instance Zoomed (StateRefT s m) = Focusing m

instance (MonadIO m) => Zoom (StateRefT s m) (StateRefT t m) s t where
  zoom l (StateRefT action) =
    StateRefT $ ReaderT $ \ref -> do
      s <- liftIO $ readIORef ref
      (v', s') <- unfocusing . flip l s $ \t -> Focusing $ do
        ref' <- liftIO (newIORef t)
        v <- runReaderT action ref'
        t' <- liftIO (readIORef ref')
        return (v, t')
      liftIO $ writeIORef ref s'
      return v'
  {-# INLINE zoom #-}
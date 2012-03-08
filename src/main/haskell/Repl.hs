import Control.Monad
import Control.Monad.Trans
import System.Console.Readline

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
  -- MaybeT     :: m (Maybe a) -> MaybeT m a
  -- runMaybeT  :: MaybeT m a  -> m (Maybe a)

instance Functor m => Functor (MaybeT m) where
  -- fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f x = MaybeT (fmap f `fmap` runMaybeT x)

instance Monad m => Monad (MaybeT m) where
  -- return :: a -> MaybeT m a
  return = MaybeT . return . return

  -- fail :: String -> MaybeT m a
  fail = MaybeT . return . fail

  -- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  x >>= f = MaybeT (runMaybeT x >>= maybe (return Nothing) (runMaybeT . f))

instance MonadTrans MaybeT where
  -- lift :: Monad m => m a -> MaybeT m a
  lift ma = MaybeT (Just `fmap` ma)
    where fmap f ma = ma >>= return . f

-- we have repl =
--   read :: IO (Maybe String)
--   eval :: String -> Maybe a
--   puts :: a -> IO ()
--   loop :: ...


module HPack.Utils
(whenM, raiseEither)
where

import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT, throwE)

-- | Like 'when' but accepts a monad as first argument
whenM :: Monad m => m Bool -> m () -> m ()
whenM s r = s >>= flip when r

-- | Handle an Either in the ExceptT monad transformer
raiseEither :: Monad m => (e -> e') -> Either e a -> ExceptT e' m a
raiseEither f (Left x)  = throwE (f x)
raiseEither f (Right x) = return x

{-# LANGUAGE GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}
module HPack.Monads
( HPackT, runHPackT
, HPackM, runHPackM
, M.MonadIO
, lift, liftIO, M.liftM, M.liftM2, M.liftM3
, mapLeft, liftEither, liftMaybe
, try, throw, catch, finally
, put, get, modify
, random, uniform, shuffle
) where

import qualified System.Random.Shuffle      as M
import qualified Control.Monad              as M
import qualified Control.Monad.IO.Class     as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader.Class as M
import qualified Control.Monad.Trans.Class  as M
import qualified Control.Monad.Trans.State  as M
import qualified Control.Monad.Trans.Except as M
import qualified Control.Monad.Random       as M
    -- (ExceptT, runExceptT, throwE, catchE, withExceptT)

-- | Monad that handles state and exceptions. Make sure it is abstract
-- by wrapping it in a datatype
newtype HPackT err state monad a
    = HPackT { unHPackT :: M.ExceptT err (M.StateT state monad) a }
    deriving (Functor, Applicative, Monad, M.MonadIO)

type HPackM err state a = HPackT err state M.Identity a

runHPackT :: Monad m => HPackT e s m a -> s -> m (Either e a)
runHPackT (HPackT m) = M.evalStateT (M.runExceptT m)

runHPackM :: HPackM e s a -> s -> Either e a
runHPackM m s = M.runIdentity (runHPackT m s)

lift :: Monad m => m a -> HPackT e s m a
lift = HPackT . M.lift . M.lift


-------------------------------------------------------
-- I/O

liftIO :: M.MonadIO m => IO a -> HPackT e s m a
liftIO = M.liftIO

runIO :: M.MonadIO m => IO (Either e a) -> HPackT e s m a
runIO m = liftIO m >>= liftEither

-------------------------------------------------------
-- Exceptions

mapLeft :: (e1 -> e2) -> Either e1 a -> Either e2 a
mapLeft f (Left e)  = Left (f e)
mapLeft f (Right x) = Right x

liftEither :: Monad m => Either e a -> HPackT e s m a
liftEither = HPackT . M.ExceptT . return

liftMaybe :: Monad m => e -> Maybe a -> HPackT e s m a
liftMaybe e (Just x) = return x
liftMaybe e Nothing  = throw e

throw :: Monad m => e -> HPackT e s m a
throw = HPackT . M.throwE

catch :: Monad m
       => HPackT e s m a
       -> (e -> HPackT e s m a)
       -> HPackT e s m a
catch (HPackT m) f = HPackT $ M.catchE m (unHPackT . f)

try :: Monad m => HPackT e s m a -> HPackT e s m (Either e a)
try = HPackT . M.lift . M.runExceptT . unHPackT

finally :: Monad m
        => HPackT e s m a
        -> (Maybe a -> HPackT e s m b)
        -> HPackT e s m b
finally m f = do
    eitherResult <- try m
    case eitherResult of
        Left err -> f Nothing
        Right x  -> f (Just x)

-------------------------------------------------------
-- State

put :: Monad m => s -> HPackT e s m ()
put = HPackT . M.lift . M.put

get :: Monad m => HPackT e s m s
get = HPackT (M.lift M.get)

modify :: Monad m => (s -> s) -> HPackT e s m ()
modify = HPackT . M.lift . M.modify

-------------------------------------------------------
-- Random

random :: M.MonadIO m => (Int, Int) -> HPackT e s m Int
random = liftIO . M.evalRandIO . M.getRandomR

uniform :: M.MonadIO m => [a] -> HPackT e s m a
uniform = liftIO . M.evalRandIO . M.uniform

shuffle :: M.MonadIO m => [a] -> HPackT e s m [a]
shuffle = liftIO . M.evalRandIO . M.shuffleM

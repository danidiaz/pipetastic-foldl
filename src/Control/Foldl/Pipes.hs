{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Foldl.Pipes (
         fromPuller,        
         fromParser, 
         fromPullerIO,
         fromParserIO 
    ) where

import qualified Control.Foldl as L
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Free
import qualified Control.Monad.Trans.Free as TF

import Pipes
import Pipes.Parse

-- A Consumer would work too.
data Feed a = Input a | EOF

type Iteratee a = Free ((->) a)  

evertedProducer :: forall a. Producer a (Iteratee (Feed a)) ()
evertedProducer = do
    r <- lift (liftF id)
    case r of
        Input a -> do
            yield a
            evertedProducer
        EOF -> return ()

type IterateeT a m = TF.FreeT ((->) a) m 

evertedProducer' :: forall a m. Monad m => Producer a (IterateeT (Feed a) m) ()
evertedProducer' = do
    r <- lift (liftF id)
    case r of
        Input a -> do
            yield a
            evertedProducer'
        EOF -> return ()

{-|
 If the puller action ends successfully with r, all further inputs to the
 fold are ignored and at the end the fold returns r.

 If the puller action fails, the fold is interrupted right away, using
 Except.
-}
fromPuller :: (forall m x. Monad m => Producer a m x -> m (Either e r)) 
           -> L.FoldM (Except e) a r
fromPuller action = L.FoldM step begin done 
  where
    begin = return (action evertedProducer)

    step i a = case i of
        x@(Pure _) -> return x
        Free f -> case f (Input a) of
            Pure (Left e) -> throwE e
            x -> return x

    done i = case i of
        Pure x -> except x
        Free f -> case f EOF of
            Pure a -> except a
            Free _ -> error "can never happen"

fromParser :: (forall m. Monad m => Parser a m (Either e r)) 
           -> L.FoldM (Except e) a r
fromParser parser = fromPuller (evalStateT parser)


fromPullerIO :: MonadIO m 
             => (forall t x. (MonadTrans t, MonadIO (t m)) => Producer a (t m) x 
                                                           -> t m (Either e r)) 
             -> L.FoldM (ExceptT e m) a r
fromPullerIO action = L.FoldM step begin done
  where
    begin = return (ExceptT (action evertedProducer'))

    step :: MonadIO m => ExceptT e (IterateeT (Feed a) m) r 
         -> a 
         -> ExceptT e m (ExceptT e (IterateeT (Feed a) m) r)
    step i a = let i' = TF.runFreeT (runExceptT i) in
        ExceptT (join (liftM (step' a) i'))

    step' a i = case i of
        TF.Pure (Left a1) -> return (Left a1)
        TF.Pure (Right a1) -> return (Right (return a1))
        TF.Free z -> liftM step'' (TF.runFreeT (z (Input a)))

    step'' ii = case ii of 
        TF.Pure (Left a1) -> Left a1 
        TF.Pure (Right a1) -> Right (return a1) 
        x -> Right (ExceptT (TF.FreeT (return x)))

    done :: MonadIO m => ExceptT e (IterateeT (Feed a) m) r 
         -> ExceptT e m r
    done i = let i' = TF.runFreeT (runExceptT i) in
        ExceptT (join (liftM done' i'))

    done' :: MonadIO m => (TF.FreeF ((->) (Feed a)) 
                                    (Either e r) 
                                    (IterateeT (Feed a) m (Either e r)))
          -> m (Either e r)  
    done' i = case i of
        TF.Pure z -> return z
        TF.Free z -> liftM done'' (TF.runFreeT (z EOF)) 

    done'' i = case i of
        TF.Pure z -> z
        TF.Free _ -> error "can never happen"


fromParserIO :: MonadIO m 
             => (forall t. (MonadTrans t, MonadIO (t m)) => Parser a (t m) (Either e r))
             -> L.FoldM (ExceptT e m) a r
fromParserIO parser = fromPullerIO (evalStateT parser)


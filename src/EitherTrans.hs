module EitherTrans where

import Control.Monad.Trans.Class

newtype EitherT e m a = EitherT { runEither :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
  fmap f = EitherT . fmap (fmap f) . runEither

instance (Applicative m) => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  f <*> a = EitherT $ fmap (<*>) uf <*> ua
      where
          uf = runEither f
          ua = runEither a


instance (Monad m) => Monad (EitherT e m) where
  return = EitherT . return . return
  a >>= f = EitherT $ do
              x <- runEither a
              case x of
                  Right r -> runEither $ f r
                  Left l -> return $ Left l

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right
{-# LANGUAGE InstanceSigs #-}
import Control.Applicative

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
 fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
 pure :: a -> Compose f g a
 pure = Compose . pure . pure

 (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
 (Compose f) <*> (Compose a) = Compose $ liftA2 (<*>) f a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
 foldMap h (Compose fga) = foldMap (foldMap h) fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
 traverse f (Compose t) = Compose <$> (traverse (traverse f) t)

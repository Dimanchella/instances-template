module Lib where

data Singleton a = Singleton a deriving (Eq, Show)
data Productish a b = Productish a b deriving (Eq, Show)
data Summish a b = First a | Second b deriving (Eq, Show)
data Optional a = NoValue | HasValue a deriving (Eq, Show)
data NotQuiteList a = Value a | Layer (NotQuiteList a) deriving (Eq, Show)
data NotEmpty a = LastValue a | MidValue a (NotEmpty a) deriving (Eq, Show)

-- Singleton

instance Functor Singleton where
  -- TODO
  fmap = undefined

instance Applicative Singleton  where
  -- TODO
  pure = undefined
  (<*>) = undefined

instance Monad Singleton where
  -- TODO
  (>>=) = undefined

instance Foldable Singleton where
  -- TODO
    foldMap = undefined
    -- или
    -- foldr = undefined

instance Traversable Singleton where
  -- TODO
  sequenceA = undefined
  -- или
  -- traverse = undefined

-- Productish

instance Functor (Productish x) where
  -- TODO
  fmap = undefined

instance (Monoid a) => Applicative (Productish a) where
  -- TODO
  pure = undefined
  (<*>) = undefined

instance (Monoid a) => Monad (Productish a) where
  -- TODO
  (>>=) = undefined

instance Foldable (Productish a) where
  -- TODO
    foldMap = undefined
    -- или
    -- foldr = undefined

instance Traversable (Productish a) where
  -- TODO
  sequenceA = undefined
  -- или
  -- traverse = undefined

-- Summish

instance Functor (Summish a) where
  -- TODO
  fmap = undefined

instance Applicative (Summish a) where
  -- TODO
  pure = undefined
  (<*>) = undefined

instance Monad (Summish a) where
  -- TODO
  (>>=) = undefined

instance Foldable (Summish a) where
  -- TODO
    foldMap = undefined
    -- или
    -- foldr = undefined

instance Traversable (Summish a) where
  -- TODO
  sequenceA = undefined
  -- или
  -- traverse = undefined


-- Optional

instance Functor Optional where
  -- TODO
  fmap = undefined

instance Applicative Optional where
  -- TODO
  pure = undefined
  (<*>) = undefined

instance Monad Optional where
  -- TODO
  (>>=) = undefined

instance Foldable Optional where
  -- TODO
    foldMap = undefined
    -- или
    -- foldr = undefined

instance Traversable Optional where
  -- TODO
  sequenceA = undefined
  -- или
  -- traverse = undefined


-- NotQuiteList

instance Functor NotQuiteList where
  -- TODO
  fmap = undefined

instance Applicative NotQuiteList where
  -- TODO
  pure = undefined
  (<*>) = undefined

instance Monad NotQuiteList where
  -- TODO
  (>>=) = undefined

instance Foldable NotQuiteList where
  -- TODO
    foldMap = undefined
    -- или
    -- foldr = undefined

instance Traversable NotQuiteList where
  -- TODO
  sequenceA = undefined
  -- или
  -- traverse = undefined

-- NotEmpty

instance Functor NotEmpty where
  -- TODO
  fmap = undefined

instance Applicative NotEmpty where
  -- TODO
  pure = undefined
  (<*>) = undefined

instance Monad NotEmpty where
  -- TODO
  (>>=) = undefined

instance Foldable NotEmpty where
  -- TODO
    foldMap = undefined
    -- или
    -- foldr = undefined

instance Traversable NotEmpty where
  -- TODO
  sequenceA = undefined
  -- или
  -- traverse = undefined

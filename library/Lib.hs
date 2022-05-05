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
  fmap f (Singleton a) = Singleton $ f a

instance Applicative Singleton  where
  -- TODO
  pure = Singleton
  (Singleton f) <*> (Singleton a) = Singleton (f a)

instance Monad Singleton where
  -- TODO
  (Singleton a) >>= f = f a

instance Foldable Singleton where
  -- TODO
    foldMap f (Singleton a) = f a
    -- или
    -- foldr = undefined

instance Traversable Singleton where
  -- TODO
  sequenceA (Singleton a) = Singleton <$> a
  -- или
  -- traverse = undefined

-- Productish

instance Functor (Productish a) where
  -- TODO
  fmap f (Productish a b) = Productish a $ f b

instance (Monoid a) => Applicative (Productish a) where
  -- TODO
  pure a = Productish mempty a
  (Productish a b) <*> (Productish c d) = Productish (a <> c) (b d)

instance (Monoid a) => Monad (Productish a) where
  -- TODO
  (Productish a b) >>= f = case f b of (Productish c d) -> Productish (a <> c) d

instance Foldable (Productish a) where
  -- TODO
    foldMap f (Productish _ b) = f b
    -- или
    -- foldr = undefined

instance Traversable (Productish a) where
  -- TODO
  sequenceA (Productish a b) = Productish a <$> b
  -- или
  -- traverse = undefined

-- Summish

instance Functor (Summish a) where
  -- TODO
  fmap _ (First x) = First x
  fmap f (Second y) = Second $ f y

instance Applicative (Summish a) where
  -- TODO
  pure = Second
  First a <*> _ = First a
  Second a <*> b = fmap a b

instance Monad (Summish a) where
  -- TODO
  First a >>= _ = First a
  Second a >>= b = b a

instance Foldable (Summish a) where
  -- TODO
    foldMap _ (First _) = mempty
    foldMap f (Second b) = f b
    -- или
    -- foldr = undefined

instance Traversable (Summish a) where
  -- TODO
  sequenceA (First a) = pure $ First a
  sequenceA (Second b) = Second <$> b
  -- или
  -- traverse = undefined


-- Optional

instance Functor Optional where
    fmap _ NoValue = NoValue
    fmap f (HasValue a) = HasValue (f a)

instance Applicative Optional where
  -- TODO
  pure = HasValue
  HasValue a <*> b = fmap a b
  NoValue <*> _ = NoValue

instance Monad Optional where
  -- TODO
  (HasValue a) >>= f = f a
  NoValue >>= _ = NoValue

instance Foldable Optional where
  -- TODO
    foldMap _ NoValue = mempty
    foldMap f (HasValue a) = f a
    -- или
    -- foldr = undefined

instance Traversable Optional where
  -- TODO
  sequenceA NoValue = pure NoValue
  sequenceA (HasValue a) = HasValue <$> a
  -- или
  -- traverse = undefined


-- NotQuiteList

instance Functor NotQuiteList where
  -- TODO
  fmap f (Value a) = Value $ f a
  fmap f (Layer a) = Layer $ fmap f a

instance Applicative NotQuiteList where
  -- TODO
  pure a = Value a
  (Layer a) <*> b = Layer (a <*> b)
  (Value a) <*> (Layer b) = Layer (a <$> b)
  (Value a) <*> (Value b) = Value $ a b

instance Monad NotQuiteList where
  -- TODO
  (Value a) >>= b = b a
  (Layer a) >>= b = Layer (a >>= b)

instance Foldable NotQuiteList where
  -- TODO
  foldMap f (Value a) = f a
  foldMap f (Layer a) = foldMap f a
    -- или
    -- foldr = undefined

instance Traversable NotQuiteList where
  -- TODO
  sequenceA (Value a) = fmap Value a
  sequenceA (Layer a) = Layer <$> sequenceA a
  -- или
  -- traverse = undefined

-- NotEmpty

instance Semigroup (NotEmpty a) where
  -- (LastValue a) <> (LastValue b) = MidValue a b
  (LastValue a) <> b = MidValue a b
  (MidValue a b) <> c = MidValue a (b <> c)

instance Functor NotEmpty where
  -- TODO
  fmap f (LastValue a) = LastValue $ f a
  fmap f (MidValue a b) = MidValue (f a) $ fmap f b

instance Applicative NotEmpty where
  -- TODO
  pure a = LastValue a
  (MidValue a b) <*> (MidValue c d) = MidValue (a c) (b <*> d)
  (MidValue a b) <*> (LastValue c) = MidValue (a c) (b <*> LastValue c)
  (LastValue a) <*> (MidValue b c) = MidValue (a b) (a <$> c)
  (LastValue a) <*> (LastValue b) = LastValue $ a b

instance Monad NotEmpty where
  -- TODO
  --(>>=) = undefined
  -- (LastValue a) >>= c = c a
  -- (MidValue _ b) >>= c = foldr1 (<>) (c <$> b)
  -- (LastValue a) >>= b = b a
  -- (MidValue a b) >>= c = MidValue (c a) (b >>= c)
  b >>= f = foldr1 (<>) (f <$> b)

instance Foldable NotEmpty where
  -- TODO
    foldMap f (LastValue a) = f a
    foldMap f (MidValue a b) = (f a) <> foldMap f b
    -- или
    --foldr = undefined

instance Traversable NotEmpty where
  -- TODO
  -- sequenceA = undefined
  -- sequenceA (LastValue a) = fmap LastValue a
  -- sequenceA (MidValue a b) = MidValue <$> fmap LastValue a <*> sequenceA b
  -- или
  traverse f (LastValue a) = LastValue <$> f a
  traverse f (MidValue a b) = MidValue <$> f a <*> traverse f b

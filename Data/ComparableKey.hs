{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
module Data.ComparableKey 
  ( EqByKey (..)
  , OrdByKey (..)
  , EqKeyed (..)
  , OrdKeyed (..)
  , HashableByKey (..)
  , HashKeyed (..)
  ) where

import Prelude
import Data.Int
import Data.Word
import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as TL (Text)
import Data.Hashable

#include "MachDeps.h"

-- | Notion that a record has a unique key (that can be used to remove duplicates and compare records)
-- TODO: Use an existing notion of keyed (hashable) data structure?
--       Data.Hashable wasn't used because only the keyable portion is hashed
--       Data.Key wasn't used because it wasn't clear how to create functor instances for the records in question...
--       However, this last option (or perhaps a class from Lens) seems most promising
{-
class EqByKey a b where
  eqByKey :: a -> b -> Bool
  default eqByKey :: Eq a => a -> b -> Bool
  eqByKey = (==)

instance EqByKey b a => EqByKey a b where
  eqByKey :: a -> b -> Bool
  x `eqByKey` y = y `eqByKey` x

newtype  EqByKey a a => EqKeyed a = EqKeyed a
instance EqByKey a a => Eq (EqKeyed a) (EqKeyed a) where
  (EqKeyed x) == (EqKeyed y) = x `eqByKey` y
instance EqByKey a a => EqByKey (a,b) (a,b) where
  (x, _) `eqByKey` (y, _) = x `eqByKey` y
instance EqByKey a a => EqByKey (Maybe a) (Maybe a) where
  Nothing  `eqByKey` Nothing  = True
  Nothing  `eqByKey` _        = False
  _        `eqByKey` Nothing  = False
  (Just x) `eqByKey` (Just y) = x `eqByKey` y
instance (EqByKey a a, EqByKey b b) => EqByKey (Either a b) (Either a b) where
  (Left _)  `eqByKey` (Right _) = False
  (Right _) `eqByKey` (Left _)  = False
  (Left x)  `eqByKey` (Left y)  = x `eqByKey` y
  (Right x) `eqByKey` (Right y) = x `eqByKey` y
-}

class EqByKey a where
  eqByKey :: a -> a -> Bool
  default eqByKey :: Eq a => a -> a -> Bool
  eqByKey = (==)

newtype  EqByKey a => EqKeyed a = EqKeyed a
instance EqByKey a => Eq (EqKeyed a) where
  (EqKeyed x) == (EqKeyed y) = x `eqByKey` y
instance EqByKey a  => EqByKey (a,b) where
  (x, _) `eqByKey` (y, _) = x `eqByKey` y
instance EqByKey a => EqByKey (Maybe a) where
  Nothing  `eqByKey` Nothing  = True
  Nothing  `eqByKey` _        = False
  _        `eqByKey` Nothing  = False
  (Just x) `eqByKey` (Just y) = x `eqByKey` y
instance (EqByKey a, EqByKey b) => EqByKey (Either a b) where
  (Left _)  `eqByKey` (Right _) = False
  (Right _) `eqByKey` (Left _)  = False
  (Left x)  `eqByKey` (Left y)  = x `eqByKey` y
  (Right x) `eqByKey` (Right y) = x `eqByKey` y
instance EqByKey Int
instance EqByKey Int8
instance EqByKey Int16
instance EqByKey Int32
instance EqByKey Int64
instance EqByKey Word
instance EqByKey Word8
instance EqByKey Word16
instance EqByKey Word32
instance EqByKey Word64
instance EqByKey Char
instance EqByKey T.Text
instance EqByKey TL.Text

class EqByKey a => OrdByKey a where 
  compareByKey :: a -> a -> Ordering
  default compareByKey :: Ord a => a -> a -> Ordering
  compareByKey = compare
newtype  OrdByKey a => OrdKeyed a = OrdKeyed a
instance OrdByKey a => Eq (OrdKeyed a) where
  (OrdKeyed x) == (OrdKeyed y) = x `eqByKey` y
instance OrdByKey a => Ord (OrdKeyed a) where
  (OrdKeyed x) `compare` (OrdKeyed y) = x `compareByKey` y
instance OrdByKey a => OrdByKey (a,b) where
  (x, _) `compareByKey` (y, _) = x `compareByKey` y
instance OrdByKey a => OrdByKey (Maybe a) where
  Nothing  `compareByKey` Nothing  = EQ
  Nothing  `compareByKey` _        = LT
  _        `compareByKey` Nothing  = GT
  (Just x) `compareByKey` (Just y) = x `compareByKey` y
instance (OrdByKey a, OrdByKey b) => OrdByKey (Either a b) where
  (Left _)  `compareByKey` (Right _) = Left () `compare` Right ()
  (Right _) `compareByKey` (Left _)  = Right () `compare` Left ()
  (Left x)  `compareByKey` (Left y)  = x `compareByKey` y
  (Right x) `compareByKey` (Right y) = x `compareByKey` y
instance OrdByKey Int
instance OrdByKey Int8
instance OrdByKey Int16
instance OrdByKey Int32
instance OrdByKey Int64
instance OrdByKey Word
instance OrdByKey Word8
instance OrdByKey Word16
instance OrdByKey Word32
instance OrdByKey Word64
instance OrdByKey Char
instance OrdByKey T.Text
instance OrdByKey TL.Text

-- | A default salt used in the implementation of hashByKey (See Data.Hashable.Class)
defaultSalt :: Int
#if WORD_SIZE_IN_BITS < 64
defaultSalt = 0x087fc72c
#else
defaultSalt = 0xdc36d1615b7400a4
#endif
{-# INLINE defaultSalt #-}

-- | A version of Hashable that operates only on the keys portion of a data structure
class HashableByKey a where 
  hashByKey :: a -> Int
  hashByKey = hashWithSaltByKey defaultSalt

  hashWithSaltByKey :: Int -> a -> Int
  default hashWithSaltByKey :: Hashable a => Int -> a -> Int
  hashWithSaltByKey = hashWithSalt

newtype  HashableByKey a => HashKeyed a = HashKeyed a 
instance HashableByKey a => Hashable (HashKeyed a) where
  hashWithSalt salt (HashKeyed x) = hashWithSaltByKey salt x
instance HashableByKey a => HashableByKey (a, b) where
  hashWithSaltByKey salt = hashWithSaltByKey salt . fst
instance HashableByKey Int
instance HashableByKey Int8
instance HashableByKey Int16
instance HashableByKey Int32
instance HashableByKey Int64
instance HashableByKey Word
instance HashableByKey Word8
instance HashableByKey Word16
instance HashableByKey Word32
instance HashableByKey Word64
instance HashableByKey Char
instance HashableByKey T.Text
instance HashableByKey TL.Text

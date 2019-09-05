
-- |
-- This package provides functionality for equality by only Constructors.
-- That means ConstrEq is ignore all parameters of a constructor and only makes a difference based on the constructor.
--
-- Let us look at an example:
--
-- @
-- data T = T1 | T2 Int | T3 Int
--   deriving 'Generic'
-- 
-- instance ConstrEq T
--
-- constrEq T1 (T2 0)     == False
-- constrEq T1 T1         == True
-- constrEq (T2 1) (T2 2) == True
-- constrEq (T2 3) (T3 3) == False
-- @
-- 


module Generics.Deriving.ConstrEq where

import GHC.Generics

import Data.Int
import System.IO (BufferMode, Handle, HandlePosn, IOMode, SeekMode)
import System.IO.Error (IOErrorType)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr
import Foreign.StablePtr (StablePtr)




class ConstrEq' f where
    constrEq' :: f p -> f p -> Bool


instance (ConstrEq' f) => ConstrEq' (M1 i t f) where
    constrEq' (M1 a) (M1 b) = constrEq' a b

instance (ConstrEq f) => ConstrEq' (K1 i f) where
    constrEq' (K1 a) (K1 b) = constrEq a b

instance ConstrEq' V1 where
    constrEq' _ _ = False

instance ConstrEq' U1 where
    constrEq' _ _ = True
    
instance (ConstrEq' f, ConstrEq' g) => ConstrEq' (f :+: g) where
    constrEq' (L1 _) (L1 _) = True
    constrEq' (R1 a) (R1 b) = constrEq' a b
    constrEq' _ _ = False

instance ConstrEq' (f :*: g) where
    constrEq' _ _ = True
    

class ConstrEq a where
    constrEq :: a -> a -> Bool
    default constrEq :: (Generic a, ConstrEq' (Rep a)) => a -> a -> Bool
    constrEq a b = constrEq' (from a) (from b)




    

instance ConstrEq Int where
  constrEq = (==)

instance ConstrEq Int8 where
  constrEq = (==)

instance ConstrEq Int16 where
  constrEq = (==)

instance ConstrEq Int32 where
  constrEq = (==)

instance ConstrEq Int64 where
  constrEq = (==)

instance ConstrEq Integer where
  constrEq = (==)

instance ConstrEq IntPtr where
  constrEq = (==)

instance ConstrEq IOError where
  constrEq = (==)

instance ConstrEq IOErrorType where
  constrEq = (==)

instance ConstrEq IOMode where
  constrEq = (==)

instance ConstrEq BufferMode where
  constrEq = (==)

instance ConstrEq Handle where
  constrEq = (==)

instance ConstrEq HandlePosn where
  constrEq = (==)

instance ConstrEq SeekMode where
  constrEq = (==)

instance ConstrEq (ForeignPtr a) where
  constrEq = (==)

instance ConstrEq (StablePtr a) where
  constrEq = (==)

instance ConstrEq Char where
  constrEq = (==)


ceqdefault :: (Generic a, ConstrEq' (Rep a)) => a -> a -> Bool
ceqdefault x y = constrEq' (from x) (from y)

instance ConstrEq a => ConstrEq [a] where
  constrEq = ceqdefault

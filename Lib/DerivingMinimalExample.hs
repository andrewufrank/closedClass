-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass,
  GeneralizedNewtypeDeriving, DefaultSignatures, DerivingStrategies,
  TypeOperators, ConstrainedClassMethods #-}


module Lib.DerivingMinimalExample
where

import           GHC.Generics
import           Uniform.Strings
import           Data.Text

class  Zeros a where
    zero :: a
    default zero :: (Generic a, GZero (Rep a)) => a
    zero = to gzero

    isZero :: Eq a => a -> Bool
    isZero = (zero ==)

class GZero a  where
    gzero :: a x
instance GZero U1 where
    gzero = U1
instance Zeros a => GZero (K1 i a) where
    gzero = K1 zero
instance (GZero a, GZero b) => GZero (a :*: b) where
    gzero = gzero :*: gzero
instance GZero a => GZero (M1 i c a) where
    gzero = M1 gzero


data B1 = B1 Int
     deriving   (Show, Read, Eq, Ord, Generic, Zeros)
newtype Ax = Ax Text
     deriving  (Show, Read, Eq, Ord, Generic, Zeros)


instance Zeros Int where zero = 0
instance Zeros Text where zero = ""

deriveTest2 :: IO ()
deriveTest2 = do
    putIOwords ["deriveTest2"]
    return ()

deriveTest3 = do
    putIOwords ["deriveTest3"]
    return ()





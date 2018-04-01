-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE ScopedTypeVariables   #-}
--{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric
    , DeriveAnyClass
    , GeneralizedNewtypeDeriving
    , StandaloneDeriving
    , DefaultSignatures
    , DerivingStrategies
    , TypeOperators
         #-}

module Lib.DerivingMinimalExample
    where

import GHC.Generics

import Data.Text

class   Zeros a where
    zero :: a
    default zero :: (Generic a, GZero (Rep a)) => a
    zero = to gzero

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


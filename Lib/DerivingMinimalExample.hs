-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}
--{-# LANGUAGE FlexibleContexts      #-}
--{-# LANGUAGE FlexibleInstances     #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE ScopedTypeVariables   #-}
--{-# LANGUAGE TypeFamilies          #-}
--{-# LANGUAGE TypeSynonymInstances  #-}
--{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric
    , DeriveAnyClass
    , GeneralizedNewtypeDeriving
    , StandaloneDeriving
    , DefaultSignatures
    , DerivingStrategies
         #-}

module Lib.DerivingMinimalExample
    where

import GHC.Generics

class   Zeros z where
    zero :: z
    default zero :: (Generic z, Gzero (Rep z)) => z
    zero = gzero (from z)

instance Generic Int

class Gzero f  where
    gzero :: f a -> a
instance Gzero (Rec0 Int) where
    gzero (Rec0 i a) = a


data B1 = B1 Int
     deriving stock (Show, Read, Eq, Ord, Generic)
deriving instance Zeros B1


instance Zeros Int where zero = 0



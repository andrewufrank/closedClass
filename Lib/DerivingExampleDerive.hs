-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# LANGUAGE DeriveGeneric
    , DeriveAnyClass
    , GeneralizedNewtypeDeriving
    , DerivingStrategies
    , StandaloneDeriving
     #-}
module Lib.DerivingExampleDerive
    where


import           Test.Framework
import Data.Text
import Lib.DerivingExample
import GHC.Generics
import Data.Generics.Text
import Data.Monoid

newtype Basis1 = Basis1 String deriving newtype (Show, Read, Eq, Ord, Generic, Zeros)
--deriving instance Generic (Zeros Text) => Generic Basis1

--instance Zeros Basis1 where zero = Basis1 zero

newtype Token = Token {token:: Basis1} deriving newtype (Show, Read, Eq, Ord, Zeros)

--instance Zeros Token where zero = Token zero

newtype Ax = Ax Text
     deriving newtype (Show, Read, Eq, Ord, Zeros)

data Bx = B1 Int
     deriving stock (Show, Read, Eq, Ord, Generic)  --not Zeros
--     deriving anyclass (Show, Read, Eq, Ord, Generic, Zeros)
     -- anyclass gives no methods
     -- stock works not for "my" classes

--instance Generic (Bx) where
--    type Rep (Bx) = RepBx
--    from Bx = U1
--    to U1 = Bx



    --


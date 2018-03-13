-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass
--    , GeneralizedNewtypeDeriving
     #-}
module Lib.DerivingExampleDerive
    where


import           Test.Framework
import Data.Text
import Lib.DerivingExample
import GHC.Generics

newtype Basis1 = Basis1 Text deriving (Show, Read, Eq, Ord, Generic)

instance Zeros Basis1 where zero = Basis1 zero

newtype Token = Token {token:: Basis1} deriving (Show, Read, Eq, Ord, Generic, Zeros)




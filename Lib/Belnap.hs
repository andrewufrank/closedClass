-----------------------------------------------------------------------------
--
-- Module      :  the distinction algebra
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveAnyClass     #-}

module Belnap (BelnapLogic (..)
    , htf_Belnap_thisModulesTests
    , partialEQ
    )
    where

import Test.Framework
import Uniform.Strings
import Uniform.Error
--import GHC.Generic

import  Data.PartialOrd (PartialOrd, partialLEQ, partialEQ, partialLT, partialGT)


data BelnapLogic = Aff | Rej |  Top | Bot deriving (Eq, Show, Read, Ord, Enum, Bounded)

        -- affirmativ, reject = negative, indiffirent = not apply, contradiction
        -- ord used for standard operations, eg. sort

-- not4 :: BelnapLogic -> BelnapLogic
instance  Arbitrary BelnapLogic where
    arbitrary = arbitraryBoundedEnum


instance PartialOrd BelnapLogic where
    partialLEQ _ Top = True
    partialLEQ  Bot _  = True
    partialLEQ _ _ = False

prop_eq :: BelnapLogic -> BelnapLogic -> Bool
prop_eq a b = (partialEQ a b) == (partialEQ b a )

prop_lt_gt :: BelnapLogic -> BelnapLogic -> Bool
prop_lt_gt a b = (partialLT a b) == ( (partialGT b a))


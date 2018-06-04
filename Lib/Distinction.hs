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

module Distinction
    where

import Test.Framework
import Uniform.Strings
import Uniform.Error
--import GHC.Generic
import  Data.PartialOrd (PartialOrd, partialLEQ, partialEQ, partialLT, partialGT)

class Distinctions d where
    sub, super :: d -> d -> Bool
--    order :: d -> d -> Ordering  -- use compare from Ord
    join, meet :: d -> d -> d

data BelnapLogic = Aff | Rej |  Top | Bot deriving (Eq, Show, Read, Enum, Bounded)

        -- affirmativ, reject = negative, indiffirent = not apply, contradiction

-- not4 :: BelnapLogic -> BelnapLogic
instance  Arbitrary BelnapLogic where
    arbitrary = arbitraryBoundedEnum


instance PartialOrd BelnapLogic where
--    partialLEQ Top _ = False
    partialLEQ _ Top = True
    partialLEQ  Bot _  = True
    partialLEQ _ _ = False
--    partialLEQ _ Bot = False
--    partialLEQ  Aff Rej = False
--    partialLEQ  Rej Aff = False
--    partialLEQ Aff Aff = False
--    partialLEQ Rej Rej = False
--    partialLEQ a b = errorT ["partialLEQ missing", showT a , showT b]
--    (=>)  Top Top = True
--    (=>)  Top Top = True
--    (=>)  Top Top = True
--    (=>)  Top Top = True
--    (=>)  Top Top = True
--    (=>)  Top Top = True
--    compare Top _   = GT
--    compare Aff Top = LT
--    compare Aff Aff = EQ
--    compare Aff Rej = EQ
--    compare Aff _  = LT
--    compare Rej Top = LT
--    compare Rej Rej = EQ
--    compare Rej Aff = EQ
--    compare Rej _   = LT
--    compare Bot Bot = EQ

--turnOrdering :: Ordering -> Ordering
--turnOrdering EQ = EQ
--turnOrdering GT = LT
--turnOrdering LT = GT


prop_eq :: BelnapLogic -> BelnapLogic -> Bool
prop_eq a b = (partialEQ a b) == (partialEQ b a )

prop_leq_eq :: BelnapLogic -> BelnapLogic -> Bool
prop_leq_eq a b = (partialLT a b) == ( (partialGT a b))

prop_leq :: BelnapLogic -> BelnapLogic -> Bool
prop_leq a b = (partialLEQ a b) == (partialEQ a b || not (partialLEQ b a))

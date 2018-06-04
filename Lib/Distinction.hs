-----------------------------------------------------------------------------
--
-- Module      :  the distinction algebra
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
{-# LANGUAGE DeriveAnyClass     #-}

module Distinction
    where

import Test.Framework
--import GHC.Generic
import Data.PartialOrd

class Distinctions d where
    sub, super :: d -> d -> Bool
--    order :: d -> d -> Ordering  -- use compare from Ord
    join, meet :: d -> d -> d

data BelnapLogic = Aff | Rej |  Top | Bot deriving (Eq, Show, Read, Enum, Bounded)

        -- affirmativ, reject = negative, indiffirent = not apply, contradiction

-- not4 :: BelnapLogic -> BelnapLogic
instance  Arbitrary BelnapLogic where
    arbitrary = arbitraryBoundedEnum


instance Ord BelnapLogic where
    compare Top Top = EQ
    compare Top _   = GT
    compare Aff Top = LT
    compare Aff Aff = EQ
    compare Aff Rej = EQ
    compare Aff _  = LT
    compare Rej Top = LT
    compare Rej Rej = EQ
    compare Rej Aff = EQ
    compare Rej _   = LT
    compare Bot Bot = EQ

turnOrdering :: Ordering -> Ordering
turnOrdering EQ = EQ
turnOrdering GT = LT
turnOrdering LT = GT

prop_0 :: BelnapLogic -> BelnapLogic -> Bool
prop_0 a b = (turnOrdering $ compare a b) == (compare b a)

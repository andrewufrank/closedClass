-----------------------------------------------------------------------------
--
-- Module      :  distinction values
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
{-# LANGUAGE DeriveAnyClass     #-}

module Distinction (Lattice (..), LatticeTests(..)
    , Distinction, DistPaper (..)
    , DistValue (..), dv2pair, pair2dv, showDV
    , B4val (..), PartialRel(..)
    , physObj'
    , human', stuff'
    , htf_Distinction_thisModulesTests
    )
    where

import Test.Framework
import Uniform.Strings
import Uniform.Error
--import qualified Data.PartialOrd as PO
--import GHC.Generic

import Belnap

-- | the distinctions in the paper (reduced)
data DistPaper = PhysObj | Human | Liquid | Edible | Tool
--            | Dairy | Cuttlery | Fermented
                deriving (Show, Read, Eq, Ord, Enum, Bounded)

type Distinction = DistPaper

instance Arbitrary DistPaper where
    arbitrary = arbitraryBoundedEnum

data DistValue dist = DV {d :: dist, v:: B4val} -- | DVtop | DVbot
            deriving (Show, Read, Eq, Ord)
            -- only True4 and Fals4 should occur, rest is always mapped to DVtop or DVbot?

----instance Show DistValue where
showDV :: Show d => DistValue d -> Text
showDV (DV d v) = s2t $  (show d) ++ s
        where s = case v of
                    True4 -> "+"
                    False4 -> "-"
                    Both4 -> "X"
                    None4 -> "*"


instance Arbitrary d => Arbitrary (DistValue d) where
    arbitrary =   fmap normalize $ fmap pair2dv arbitrary  -- (arbitrary, arbitrary)

-- these are raw distinction values, not taxa
physObj' = DV PhysObj True4
human' = DV Human True4
stuff' = DV Human False4
liquid' = DV Liquid True4
top' = top :: DistValuePaper
bot' = bottom :: DistValuePaper

test_join1 = assertEqual physObj' (ljoin physObj' physObj')
test_join2 = assertEqual (DV{d = Human, v = None4}) (ljoin human' stuff')

dv2pair (DV d v) = (d,v)  -- wie convert DVtop DVbot ??
--dv2pair DVtop = (minBound, None4)
--dv2pair DVbot = (minBound, Both4)
--dv2pair a = errorT ["dv2pair for", showT a]
pair2dv (d,v) = DV d v
--pair2dv (minBound, None4) = DVtop
--pair2dv (minBound, Both4) = DVbot
--pair2dv a = errorT ["pair2dv for", showT a]

prop_inv1 :: DistValuePaper -> Bool
prop_inv1 a = a == (pair2dv . dv2pair $ a)

prop_inv2 :: (Distinction,B4val) -> Bool
prop_inv2 a = a == (dv2pair . pair2dv $ a)

instance (Eq d, Bounded d) => Lattice (DistValue d) where
    lcompare2 a@(DV d1 v1) b@(DV d2 v2) = if d1 == d2 then lcompare v1 v2 else INC
    lmeet2  (DV d1 v1)  (DV d2 v2) = if d1 == d2 then normalize(DV d1 (lmeet v1 v2)) else bottom
    ljoin2  (DV d1 v1)  (DV d2 v2) = if d1 == d2 then normalize(DV d1 (ljoin v1 v2)) else top
    top = pair2dv (minBound, None4)  -- alternative DVtop
    bottom = pair2dv (minBound, Both4)   -- DVbot

normalize a@(DV d v) = a -- if v==bottom then bottom else if v==top then top else a

instance (Eq d, Bounded d) =>  LatticeTests (DistValue d)

test_1 = assertEqual (lsub human' human')  False
test_2 = assertEqual (lsub human' top') True
test_3 = assertEqual (lsub human' stuff') False

test_4 = assertEqual (DV{d = Human, v = Both4}) (lmeet human' stuff')
test_5 = assertEqual (DV{d = Human, v = None4}) (ljoin human' stuff')

test_6 = assertEqual (lmeet human' liquid') bottom
test_7 = assertEqual (ljoin human' liquid') top
--    prop_ass1 a b c = lmeet a (lmeet b c) ==  lmeet(lmeet a b)  c

type DistValuePaper = DistValue DistPaper

prop_compSymx :: DistValuePaper -> DistValuePaper -> Bool
prop_compSymx = prop_compSym -- lcompare a b == reverse4 (lcompare b a)

prop_ide1x, prop_ide2x :: DistValuePaper -> Bool
prop_ide1x = prop_ide1
prop_ide2x = prop_ide2
--
prop_id1x, prop_id2x :: DistValuePaper -> Bool
prop_id1x = prop_id1  -- ljoin a a == a
prop_id2x = prop_id2
--
test_9 = assertEqual (ljoin liquid' liquid') liquid'

prop_ass1x, prop_ass2x :: DistValuePaper -> DistValuePaper -> DistValuePaper -> Bool
prop_ass1x = prop_ass1 -- lmeet a (lmeet b c) ==  lmeet(lmeet a b)  c
prop_ass2x = prop_ass2 -- ljoin a (ljoin b c) == ljoin (ljoin a b)  c

prop_abs1x, prop_abs2x :: DistValuePaper -> DistValuePaper ->  Bool
prop_abs1x = prop_abs1
prop_abs2x = prop_abs2




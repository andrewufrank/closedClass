-----------------------------------------------------------------------------
--
-- Module      :  the distinction from the paper (reduced)
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

module PaperTaxa
    where

import Test.Framework
import Uniform.Strings
import Uniform.Error

import Taxon
--import qualified Data.Map as Map
--import Data.List

-- | the distinctions in the paper (reduced)
data DistPaper = PhysObj | Human | Liquid | Edible | Tool
--            | Dairy | Cuttlery | Fermented
                deriving (Show, Read, Eq, Ord, Enum, Bounded)

type Distinction = DistPaper

instance Arbitrary (DistValue DistPaper) where
    arbitrary = do
        d :: DistPaper <- arbitraryBoundedEnum
        v :: B4val <- arbitraryBoundedEnum
        return (DV d v)


ph = taxon2dvList physObj
t = zipWith ljoin ph ph
test_x = assertEqual ph t

type TaxonPaper = Taxon DistPaper

type DistValuePaper = DistValue DistPaper

-- these are raw distinction values, not taxa
physObj' = DV PhysObj True4
human' = DV Human True4
stuff' = DV Human False4
liquid' = DV Liquid True4
top' = top :: DistValuePaper
bot' = bottom :: DistValuePaper

test_1 = assertEqual (lsub human' human')  False
test_2 = assertEqual (lsub human' top') True
test_3 = assertEqual (lsub human' stuff') False

test_4 = assertEqual (DV{d = Human, v = Both4}) (lmeet human' stuff')
test_5 = assertEqual (DV{d = Human, v = None4}) (ljoin human' stuff')

test_6 = assertEqual (lmeet human' liquid') bottom
test_7 = assertEqual (ljoin human' liquid') top
--    prop_ass1 a b c = lmeet a (lmeet b c) ==  lmeet(lmeet a b)  c

test_join1 = assertEqual physObj' (ljoin physObj' physObj')
test_join2 = assertEqual (DV{d = Human, v = None4}) (ljoin human' stuff')

prop_inv1 :: DistValuePaper -> Bool
prop_inv1 a = a == (pair2dv . dv2pair $ a)

prop_inv2 :: (DistValuePaper,B4val) -> Bool
prop_inv2 a = a == (dv2pair . pair2dv $ a)

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

test_join1t = assertEqual physObj (ljoin physObj physObj)
test_join_milk = assertEqual someFood (ljoin milk bread)
    -- [(PhysObj, True4), (Human, False4), (Liquid, None4),
    --   (Edible, True4), (Tool, None4)]
someFood = specializeList [(PhysObj, True4), (Human, False4), (Liquid, None4),
              (Edible, True4), (Tool, None4)] ttop
prop_comm1x :: TaxonPaper -> TaxonPaper -> Bool
prop_comm1x   = prop_comm1
prop_comm2x :: TaxonPaper -> TaxonPaper -> Bool
prop_comm2x = prop_comm2

prop_ass1t, prop_ass2t :: TaxonPaper -> TaxonPaper -> TaxonPaper -> Bool
prop_ass1t = prop_ass1
prop_ass2t = prop_ass2
--
prop_abs1t, prop_abs2t :: TaxonPaper -> TaxonPaper ->  Bool
prop_abs1t = prop_abs1
prop_abs2t = prop_abs2
--
prop_id1t, prop_id2t :: TaxonPaper -> Bool
prop_id1t = prop_id1
prop_id2t = prop_id2
--
prop_ide1t, prop_ide2t :: TaxonPaper -> Bool
prop_ide1t = prop_ide1
prop_ide2t = prop_ide2
--
prop_compSymt :: TaxonPaper -> TaxonPaper -> Bool
prop_compSymt = prop_compSym


tbottom = bottom :: TaxonPaper
ttop = top :: TaxonPaper

test_top = assertEqual "PhysObj* Human* Liquid* Edible* Tool*" (showTaxon ttop)
test_bottom = assertEqual "PhysObjX HumanX LiquidX EdibleX ToolX" (showTaxon tbottom)
test_isBottom = assertEqual True (isBottom tbottom)
test_isBottomF = assertEqual False (isBottom ttop)
test_isTop = assertEqual True (isTop ttop)
test_isTopF = assertEqual False (isTop tbottom)


physObj = specialize PhysObj True4 ttop
human = specialize Human True4 physObj
stuff = specialize Human False4 physObj
edible = specialize Edible True4 stuff
liquid = specialize Liquid True4 stuff
milk = specialize Edible True4 liquid
bread = specialize Edible True4 foodstuff
foodstuff = specialize Tool False4 solid
solid = specialize Liquid False4 stuff

test_0t = assertEqual  "PhysObj+ Human* Liquid* Edible* Tool*" (showTaxon physObj)
test_1t = assertEqual  "PhysObj+ Human+ Liquid* Edible* Tool*" (showTaxon human)
test_2t = assertEqual  "PhysObj+ Human- Liquid* Edible* Tool*" (showTaxon stuff)
test_edible = assertEqual "PhysObj+ Human- Liquid* Edible+ Tool*"
            (showTaxon (edible::Taxon DistPaper))
test_milk = assertEqual "PhysObj+ Human- Liquid+ Edible+ Tool*"
            (showTaxon (milk::Taxon DistPaper))
test_liquid = assertEqual "PhysObj+ Human- Liquid+ Edible* Tool*"
            (showTaxon (liquid::Taxon DistPaper))

test_PEQ1 = assertEqual PEQ (lcompare physObj physObj)
test_PEQ2 = assertEqual INC (lcompare human stuff)
test_PEQ3 = assertEqual PLT (lcompare stuff physObj)
test_PEQ4 = assertEqual PGT (lcompare physObj human)
test_PEQ5 = assertEqual INC (lcompare human edible)
test_PEQ6 = assertEqual PLT (lcompare milk edible)
test_PEQ7 = assertEqual PGT (lcompare liquid milk)





-----------------------------------------------------------------------------
--
-- Module      :  the distinction which define a taxon
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

module Taxon
    where

import Test.Framework
import Uniform.Strings
import Uniform.Error

import Distinction
import qualified Data.Map as Map
import Data.List

newtype Taxon = Taxon (Map.Map Distinction B4val)  deriving (Show, Read, Eq)
unTaxon (Taxon m) = m

class Taxons t where
    specialize :: Distinction -> B4val -> t -> t
    showTaxon :: t -> Text


instance Taxons Taxon where
    specialize d v t = Taxon . Map.insert d v . unTaxon $ t
    showTaxon t =  s2t . unwords . map showDV
                                . taxon2dvList  $ t

specializeList :: [(Distinction, B4val)] -> Taxon -> Taxon
specializeList l t1= foldl (\t (d,v) -> specialize d v t) t1  l

dvList2taxon :: [DistValue] -> Taxon
dvList2taxon   =  Taxon . Map.fromList .   (map  dv2pair)

taxon2dvList :: Taxon -> [DistValue]
taxon2dvList   =        map pair2dv . Map.toAscList . unTaxon

instance Lattice Taxon where
    lcompare = compareTaxon
    lmeet = meetTaxon
    ljoin = joinTaxon
    top =  dvList2taxon (makeAll None4)-- []
    bottom = dvList2taxon (makeAll Both4)
    isBottom = any (isBottom . v) . taxon2dvList
    isTop = all (isTop . v) . taxon2dvList

makeAll v = map (\d -> DV d v) allDist :: [DistValue]
    where
            allDist = [minBound .. maxBound] :: [DistPaper]

joinTaxon :: Taxon -> Taxon -> Taxon
joinTaxon a b = dvList2taxon l
    where
        aa = taxon2dvList a :: [DistValue]
        bb = taxon2dvList b :: [DistValue]
        l = zipWith ljoin aa bb :: [DistValue]

meetTaxon :: Taxon -> Taxon -> Taxon
meetTaxon a b = dvList2taxon l
    where
        aa = taxon2dvList a :: [DistValue]
        bb = taxon2dvList b :: [DistValue]
        l = zipWith lmeet aa bb :: [DistValue]

compareTaxon :: Taxon -> Taxon -> PartialRel
compareTaxon a b = if a==b then PEQ
                    else if a==l then PGT
                    else if b==l then PLT
                    else INC
    where l = joinTaxon a b

ph = taxon2dvList physObj
t = zipWith ljoin ph ph
test_x = assertEqual ph t



test_join1 = assertEqual physObj (joinTaxon physObj physObj)
test_join_milk = assertEqual someFood (joinTaxon milk bread)
    -- [(PhysObj, True4), (Human, False4), (Liquid, None4),
    --   (Edible, True4), (Tool, None4)]
someFood = specializeList [(PhysObj, True4), (Human, False4), (Liquid, None4),
              (Edible, True4), (Tool, None4)] ttop
prop_comm1x :: Taxon -> Taxon -> Bool
prop_comm1x   = prop_comm1
prop_comm2x :: Taxon -> Taxon -> Bool
prop_comm2x = prop_comm2

prop_ass1x, prop_ass2x :: Taxon -> Taxon -> Taxon -> Bool
prop_ass1x = prop_ass1
prop_ass2x = prop_ass2
--
prop_abs1x, prop_abs2x :: Taxon -> Taxon ->  Bool
prop_abs1x = prop_abs1
prop_abs2x = prop_abs2
--
prop_id1x, prop_id2x :: Taxon -> Bool
prop_id1x = prop_id1
prop_id2x = prop_id2
--
prop_ide1x, prop_ide2x :: Taxon -> Bool
prop_ide1x = prop_ide1
prop_ide2x = prop_ide2
--
prop_compSymx :: Taxon -> Taxon -> Bool
prop_compSymx = prop_compSym


tbottom = bottom :: Taxon
ttop = top :: Taxon

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

test_0 = assertEqual  "PhysObj+ Human* Liquid* Edible* Tool*" (showTaxon physObj)
test_1 = assertEqual  "PhysObj+ Human+ Liquid* Edible* Tool*" (showTaxon human)
test_2 = assertEqual  "PhysObj+ Human- Liquid* Edible* Tool*" (showTaxon stuff)
test_edible = assertEqual "PhysObj+ Human- Liquid* Edible+ Tool*" (showTaxon (edible::Taxon))
test_milk = assertEqual "PhysObj+ Human- Liquid+ Edible+ Tool*" (showTaxon (milk::Taxon))
test_liquid = assertEqual "PhysObj+ Human- Liquid+ Edible* Tool*" (showTaxon (liquid::Taxon))

test_PEQ1 = assertEqual PEQ (lcompare physObj physObj)
test_PEQ2 = assertEqual INC (lcompare human stuff)
test_PEQ3 = assertEqual PLT (lcompare stuff physObj)
test_PEQ4 = assertEqual PGT (lcompare physObj human)
test_PEQ5 = assertEqual INC (lcompare human edible)
test_PEQ6 = assertEqual PLT (lcompare milk edible)
test_PEQ7 = assertEqual PGT (lcompare liquid milk)

instance LatticeTests Taxon


instance Arbitrary Taxon where
    arbitrary = do
        d :: [DistValue] <- arbitrary
        let d2 = makeAll None4 ++ d
        return $ dvList2taxon d2





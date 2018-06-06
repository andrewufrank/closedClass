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
--{-# LANGUAGE OverlappingInstances     #-}

module Taxon
    where

import Test.Framework
import Uniform.Strings
import Uniform.Error
--import GHC.Generic

import Distinction
import qualified Data.Map as Map
import Data.List

type Taxon = Maybe (Map.Map Distinction B4val)

instance Lattice Taxon where
    lcompare2 = compareTaxon
    lmeet2 = meetTaxon
    ljoin2 = joinTaxon
    top =  dvList2taxon makeAllNone4 -- []
    bottom = Nothing

makeAllNone4 = map makeOneNone4 allDist :: [DistValue]

makeOneNone4 :: Distinction -> DistValue
makeOneNone4 d = DV d None4
allDist = [minBound .. maxBound] :: [DistPaper]

instance LatticeTests Taxon
type MDistVal = (Maybe [DistValue])

instance {-# OVERLAPS #-} Arbitrary MDistVal where
    -- produce only semi-lattice, no bottom
    arbitrary = do
        r1 :: [DistValue] <-   arbitrary
        let r2 = normalizeTaxon r1
        return r2

instance {-# OVERLAPS #-} Arbitrary Taxon where
-- produces only elements for a semi lattice (no bottom)
    arbitrary = do
        r2 ::MDistVal <- suchThat arbitrary (not . isNothing)
        return . dvList2taxon . fromJustNote "arbitrary dvList2taxon" $ r2


dvList2taxon :: [DistValue] -> Taxon
dvList2taxon dvs = fmap Map.fromList .  fmap (map  dv2pair) . normalizeTaxon $ dvs

taxon2dvList :: String -> Taxon -> [DistValue]
taxon2dvList msg t =  if isBottom t then errorT ["taxon2dvList not for bottom"]
                        else map pair2dv . Map.toAscList . fromJustNote msg $ t

prop_conv1 :: Taxon -> Bool
prop_conv1 a = if isBottom a then True  -- not required, arbitrary gives no bottom
                else a == (dvList2taxon . taxon2dvList "prop_conv1" $ a)

normalizeTaxon :: [DistValue] -> Maybe [DistValue]
-- any Both4 gives bottom (Nothing) -- , removes None4
-- removes duplicates
normalizeTaxon  = fmap nub . normalizeTax
    where
        normalizeTax [] = Just []
        normalizeTax (a:as) = if a==bottom then Nothing
--                                else if a==top then normalizeTax as
                                else fmap (a :) (normalizeTax as)

showTaxon :: Taxon -> Text
showTaxon t = if isBottom t then "Bottom"
                else s2t . unwords . map showDV . filter (isTop . v) . taxon2dvList "showTaxon" $ t
-- does not handle bottom?


specialize :: Distinction -> B4val -> Taxon -> Taxon
specialize d v t =
--        Map.insert (d,v) t
        if isBottom t then errorT ["specialize for bottom not possible"
                                            , showT d, showT v, showT t]
                      else dvList2taxon l2

          where l1 = taxon2dvList "specialize" t :: [DistValue]
                l2 = (DV d v) : l1
                -- order is not relevant, is produced when extracted from map

physObj = specialize PhysObj True4 top
human = specialize Human True4 physObj
stuff = specialize Human False4 physObj
edible = specialize Edible True4 stuff
liquid = specialize Liquid True4 stuff
milk = specialize Edible True4 liquid

compareTaxon :: Taxon -> Taxon -> PartialRel
compareTaxon t1 t2 =   if isPrefixOf l1 l2
                                    then PGT
                                    else if isPrefixOf l2 l1
                                        then PLT
                                        else INC
    where
        l1 = Map.toAscList . fromJustNote "compareTaxon1" $ t1
        l2 = Map.toAscList . fromJustNote "compareTaxon2" $ t2


meetTaxon :: Taxon -> Taxon -> Taxon
meetTaxon t1 t2 = dvList2taxon l
    where
        l1 = map pair2dv . Map.toAscList . fromJustNote "meetTaxon1" $ t1
        l2 = map pair2dv . Map.toAscList . fromJustNote "meetTaxon2" $ t2
        l = meet33 l1 l2

meet33 :: [DistValue] -> [DistValue] -> [DistValue]
meet33 l1 l2  = zipWith lmeet l1 l2

joinTaxon :: Taxon -> Taxon -> Taxon
joinTaxon t1 t2 = dvList2taxon l
    where
        l1 = map pair2dv . Map.toAscList . fromJustNote "joinTaxon1" $ t1
        l2 = map pair2dv . Map.toAscList . fromJustNote "joinTaxon2" $ t2
        l = join33 l1 l2

join33 :: [DistValue] -> [DistValue] -> [DistValue]
join33 l1 l2  = l1 -- zipWith



test_0 = assertEqual  "Just (fromList [(PhysObj,True4)])" (showTaxon physObj)
test_1 = assertEqual  "Just (fromList [(PhysObj,True4),(Human,True4)])" (showTaxon human)
test_2 = assertEqual  "Just (fromList [(PhysObj,True4),(Human,False4)])" (showTaxon stuff)
test_3 = assertEqual "PhysObj* Human* Liquid* Edible* Tool*" (showTaxon (top::Taxon))
test_4 = assertEqual "Bottom" (showTaxon (bottom::Taxon))


--test_PEQ1 = assertEqual PEQ (lcompare physObj physObj)
--test_PEQ2 = assertEqual INC (lcompare human stuff)
--test_PEQ3 = assertEqual PLT (lcompare stuff physObj)
--test_PEQ4 = assertEqual PGT (lcompare physObj human)
--test_PEQ5 = assertEqual INC (lcompare human edible)
--test_PEQ6 = assertEqual INC (lcompare milk edible)
--test_PEQ7 = assertEqual PGT (lcompare liquid milk)
--
--test_2b = assertEqual PLT (lcompare human' top)
--test_2c = assertEqual PLT (lcompare human top)
--
--
--test_1a = assertEqual (lsub human human)  False
--test_2a = assertEqual (lsub human top) True
--test_3a = assertEqual (lsub human stuff) False
--
--
--test_meet1 = assertEqual (lmeet physObj physObj) physObj
----
--prop_compSymx :: Taxon -> Taxon -> Bool
--prop_compSymx = prop_compSym
----
--prop_comm1x :: Taxon -> Taxon -> Bool
--prop_comm1x   = prop_comm1  --ljoin a b == ljoin b a
--
--test_5 = assertEqual (showT (ljoin (bottom::Taxon) stuff)) "fromList []"
--
----prop_comm2x :: Taxon -> Taxon -> Bool
----prop_comm2x = prop_comm2

--prop_ass1x, prop_ass2x :: Taxon -> Taxon -> Taxon -> Bool
--prop_ass1x = prop_ass1
--prop_ass2x = prop_ass2
--
--prop_abs1x, prop_abs2x :: Taxon -> Taxon ->  Bool
--prop_abs1x = prop_abs1
--prop_abs2x = prop_abs2
--
--prop_id1x, prop_id2x :: Taxon -> Bool
--prop_id1x = prop_id1
--prop_id2x = prop_id2
--
--prop_ide1x, prop_ide2x :: Taxon -> Bool
--prop_ide1x = prop_ide1
--prop_ide2x = prop_ide2



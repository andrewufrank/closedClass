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
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveAnyClass     #-}

module Taxon
    where

import Test.Framework
import Uniform.Strings
import Uniform.Error
--import GHC.Generic

import Distinction
import qualified Data.Map as Map
import Data.List

type Taxon = Map.Map DistPaper B4val

instance Lattice Taxon where
    lcompare = compareTaxon
    lmeet = meetTaxon
    ljoin = joinTaxon
    top = dvList2taxon [top]
    bottom = dvList2taxon []

instance LatticeTests Taxon

dvList2taxon :: [DistValue] -> Taxon
dvList2taxon dvs =  Map.fromList . map dv2pair $ dvs

normalizeTaxon :: [DistValue] -> [DistValue]
normalizeTaxon [] = []
normalizeTaxon (a:as) = if a==bottom then normalizeTaxon as else a: normalizeTaxon as


compareTaxon :: Taxon -> Taxon -> PartialRel
compareTaxon t1 t2 = if l1 == l2
                        then PEQ
                        else if t1==top || t2==bottom then PGT
                        else if t1==bottom || t2==top then PLT
                        else if isPrefixOf l1 l2
                                    then PGT
                                    else if isPrefixOf l2 l1
                                        then PLT
                                        else INC
    where
        l1 = Map.toAscList t1
        l2 = Map.toAscList t2


meetTaxon :: Taxon -> Taxon -> Taxon
meetTaxon t1 t2 = dvList2taxon l
    where
        l1 = map pair2dv . Map.toAscList $ t1
        l2 = map pair2dv . Map.toAscList $ t2
        l = meet2 l1 l2

meet2 :: [DistValue] -> [DistValue] -> [DistValue]
meet2 l1 l2  = zipWith lmeet l1 l2

joinTaxon :: Taxon -> Taxon -> Taxon
joinTaxon t1 t2 = dvList2taxon l
    where
        l1 = map pair2dv . Map.toAscList $ t1
        l2 = map pair2dv . Map.toAscList $ t2
        l = join2 l1 l2

join2 :: [DistValue] -> [DistValue] -> [DistValue]
join2 l1 l2  = l1

physObj = dvList2taxon [physObj']
human = dvList2taxon [physObj', human']
stuff = dvList2taxon [physObj', stuff']

test_0 = assertEqual (showT physObj) "fromList [(PhysObj,True4)]"
test_1 = assertEqual (showT human) "fromList [(PhysObj,True4),(Human,True4)]"
test_2 = assertEqual (showT stuff) "fromList [(PhysObj,True4),(Human,False4)]"
test_3 = assertEqual (showT (top::Taxon)) "fromList [(PhysObj,None4)]"
test_4 = assertEqual (showT (bottom::Taxon)) "fromList []"


test_PEQ1 = assertEqual (compareTaxon physObj physObj) PEQ
test_PEQ2 = assertEqual (compareTaxon human stuff) INC
test_PEQ3 = assertEqual (compareTaxon stuff physObj) PLT
test_PEQ4 = assertEqual (compareTaxon physObj human) PGT

test_2b = assertEqual PLT (lcompare human' top)
test_2c = assertEqual PLT (lcompare human top)


test_1a = assertEqual (lsub human human)  False
test_2a = assertEqual (lsub human top) True
test_3a = assertEqual (lsub human stuff) False


--test_meet1 = assertEqual (lmeet physObj physObj) physObj
--
--prop_compSymx :: Taxon -> Taxon -> Bool
--prop_compSymx = prop_compSym
--
--prop_comm1x :: Taxon -> Taxon -> Bool
--prop_comm1x   = prop_comm1
--prop_comm2x :: Taxon -> Taxon -> Bool
--prop_comm2x = prop_comm2

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



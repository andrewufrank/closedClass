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

instance LatticeTests Taxon

dvList2taxon :: [DistValue] -> Taxon
dvList2taxon dvs =  Map.fromList . map dv2pair $ dvs

compareTaxon :: Taxon -> Taxon -> PartialRel
compareTaxon t1 t2 = if l1 == l2
                        then PEQ
                        else if isPrefixOf l1 l2
                                    then PGT
                                    else if isPrefixOf l2 l1
                                        then PLT
                                        else INC
    where
        l1 = Map.toAscList t1
        l2 = Map.toAscList t2


meetTaxon :: Taxon -> Taxon -> Taxon
meetTaxon t1 t2 = t1

    where
        l1 = Map.toAscList t1
        l2 = Map.toAscList t2

physObj = dvList2taxon [physObj']
human = dvList2taxon [physObj', human']
stuff = dvList2taxon [physObj', stuff']

test_0 = assertEqual (showT physObj) "fromList [(PhysObj,True4)]"
test_1 = assertEqual (showT human) "fromList [(PhysObj,True4),(Human,True4)]"
test_2 = assertEqual (showT stuff) "fromList [(PhysObj,True4),(Human,False4)]"


test_PEQ1 = assertEqual (compareTaxon physObj physObj) PEQ
test_PEQ2 = assertEqual (compareTaxon human stuff) INC
test_PEQ3 = assertEqual (compareTaxon stuff physObj) PLT
test_PEQ4 = assertEqual (compareTaxon physObj human) PGT

--test_1 = assertEqual (sub human' human')  False
--test_2 = assertEqual (sub human' top') True
--test_3 = assertEqual (sub human' stuff') False


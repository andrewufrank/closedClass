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

module Belnap (B4val (..)
    , htf_Belnap_thisModulesTests
    , partialEQ
    , PartialRel (..)
    , Lattice (..), LatticeTests (..)
    )
    where

import Test.Framework
import Uniform.Strings
import Uniform.Error
--import GHC.Generic


import  Data.PartialOrd (PartialOrd, partialLEQ, partialEQ, partialLT, partialGT)
import  qualified Data.PartialOrd as PO

class Lattice d where
    lsub, lsuper :: d -> d -> Bool
    -- | test for sub or super type
    lsub a b = case lcompare a b of
                    PLT -> True
                    _ -> False
    lsuper a b = case lcompare a b of
                    PGT -> True
                    _ -> False
    lcompare :: d -> d -> PartialRel
--    order :: d -> d -> Ordering  -- use compare from Ord
    ljoin, lmeet :: d -> d -> d
    top, bottom :: d
    (/\), (\/) :: d -> d -> d
    (/\) = lmeet
    (\/) = ljoin

class (Eq d, Lattice d) =>  LatticeTests d where
    prop_comm1 :: d -> d -> Bool
    prop_comm1 a b = ljoin a b == ljoin b a
    prop_comm2 :: d -> d -> Bool
    prop_comm2 a b = lmeet a b == lmeet b a

    prop_ass1, prop_ass2 :: d -> d -> d -> Bool
    prop_ass1 a b c = lmeet a (lmeet b c) ==  lmeet(lmeet a b)  c
    prop_ass2 a b c = ljoin a (ljoin b c) == ljoin (ljoin a b)  c

    prop_abs1, prop_abs2 :: d -> d ->  Bool
    prop_abs1 a b  = lmeet a (ljoin a b) ==  a
    prop_abs2 a b  = ljoin a (lmeet a b) == a

    prop_id1, prop_id2 :: d -> Bool
    prop_id1 a   = ljoin a a == a
    prop_id2 a = lmeet a a  == a

    prop_ide1, prop_ide2 :: d -> Bool
    prop_ide1 a   = ljoin a bottom == a
    prop_ide2 a = lmeet a top  == a

    prop_compSym :: d -> d -> Bool
    prop_compSym a b = lcompare a b == reverse4 (lcompare b a)

instance Lattice B4val where
    top = None4    -- no knowledge
    bottom = Both4  -- contradiction
    lmeet None4 a = a
    lmeet a None4 = a
    lmeet a b = if a==b then a else bottom
    ljoin Both4 a = a
    ljoin a Both4 = a
    ljoin a b = if a==b then a else top

    lcompare a b = case PO.compare a b of
                    Nothing -> INC
                    Just EQ -> PEQ
                    Just LT -> PLT
                    Just GT -> PGT

instance LatticeTests B4val

prop_comm1x :: B4val -> B4val -> Bool
prop_comm1x   = prop_comm1
prop_comm2x :: B4val -> B4val -> Bool
prop_comm2x = prop_comm2

prop_ass1x, prop_ass2x :: B4val -> B4val -> B4val -> Bool
prop_ass1x = prop_ass1
prop_ass2x = prop_ass2

prop_abs1x, prop_abs2x :: B4val -> B4val ->  Bool
prop_abs1x = prop_abs1
prop_abs2x = prop_abs2

prop_id1x, prop_id2x :: B4val -> Bool
prop_id1x = prop_id1
prop_id2x = prop_id2

prop_ide1x, prop_ide2x :: B4val -> Bool
prop_ide1x = prop_ide1
prop_ide2x = prop_ide2

prop_compSymx :: B4val -> B4val -> Bool
prop_compSymx = prop_compSym

prop_compSymx :: B4val -> B4val -> Bool
prop_compSymx = prop_compSym

--An algebraic structure (L, ||, &&), consisting of a set L
-- and two binary operations || (join), and && (meet), on L
--is a lattice if the following axiomatic identities hold for all elements a, b, c of L.
--
--Commutative laws
--a || b = b || a,
--a && b = b && a.
--    	
--Associative laws
--a || (b || c) = (a || b) || c,
--a && (b && c) = (a && b) && c.
--    	
--Absorption laws
--a || (a && b) = a,
--a && (a || b) = a.
--The following two identities are also usually regarded as axioms, even though they follow from the two absorption laws taken together.[note 1]
--
--Idempotent laws
--a || a = a,
--a && a = a.
--These axioms assert that both (L, ||) and (L, &&) are semilattices. The absorption laws, the only axioms above in which both meet and join appear, distinguish a lattice from an arbitrary pair of semilattices and assure that the two semilattices interact appropriately. In particular, each semilattice is the dual of the other.
--
--Bounded lattice
--A bounded lattice is an algebraic structure of the form (L, ||, &&, 0, 1) such that (L, ||, &&) is a lattice, 0 (the lattice's bottom) is the identity element for the join operation ||, and 1 (the lattice's top) is the identity element for the meet operation &&.
--
--Identity laws
--a || 0 = a,
--a && 1 = a.
--See semilattice for further details.



data PartialRel = PEQ | PGT | PLT | INC  deriving (Eq, Show, Read, Ord, Enum, Bounded)
-- | the four possible values for compare in a partial order

reverse4 PEQ = PEQ
reverse4 PGT = PLT
reverse4 PLT = PGT
reverse4 INC = INC

data B4val = Both4 | True4 | False4 |  None4  deriving (Eq, Show, Read, Ord, Enum, Bounded)
-- | the four logical values of Belnaps 4 valued logic B4

        -- affirmativ, reject = negative, indiffirent = not apply, contradiction
        -- ord used for standard operations, eg. sort

-- not4 :: BelnapLogic -> BelnapLogic
instance  Arbitrary B4val where
    arbitrary = arbitraryBoundedEnum


instance PartialOrd B4val where
    partialLEQ _ Both4 = True
    partialLEQ  None4 _  = True
    partialLEQ _ _ = False



prop_eq :: B4val -> B4val -> Bool
prop_eq a b = (partialEQ a b) == (partialEQ b a )

prop_lt_gt :: B4val -> B4val -> Bool
prop_lt_gt a b = (partialLT a b) == ( (partialGT b a))


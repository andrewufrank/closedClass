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
{-# LANGUAGE DeriveAnyClass     #-}

module Lattice (
     PartialRel (..), reverse4
    , Lattice (..), LatticeTests (..)
    )
    where

import Test.Framework
import Uniform.Strings
import Uniform.Error
--import GHC.Generic


data PartialRel = PEQ | PGT | PLT | INC  deriving (Eq, Show, Read, Ord, Enum, Bounded)
-- | the four possible values for compare in a partial order

reverse4 PEQ = PEQ
reverse4 PGT = PLT
reverse4 PLT = PGT
reverse4 INC = INC

class Eq d => Lattice d where
    lsub, lsuper :: d -> d -> Bool
    -- | test for sub or super type
    lsub a b = case lcompare a b of
                    PLT -> True
                    _ -> False
    lsuper a b = case lcompare a b of
                    PGT -> True
                    _ -> False
    lcompare :: d -> d -> PartialRel
    lcompare a b = if a == b
                        then PEQ
                        else if a==top || b==bottom then PGT
                        else if a==bottom || b==top then PLT
                        else lcompare2 a b
    lcompare2 :: d -> d -> PartialRel
    ljoin, lmeet :: d -> d -> d
    ljoin a b =
                if a==bottom then b else if b==bottom then a
                     else if a==top || b==top then top
                     else ljoin2 a b
    lmeet a b =
        if a==top then b else if b==top then a
                     else if a==bottom || b==bottom then bottom
                     else lmeet2 a b
    ljoin2, lmeet2 :: d -> d -> d
    -- deals with the generic case only

    top, bottom :: d
    isTop, isBottom :: d -> Bool
    isTop = (top==)
    isBottom = (bottom==)
    (/\), (\/) :: d -> d -> d
    (/\) = lmeet
    (\/) = ljoin
    {-# MINIMAL lcompare2, ljoin2, lmeet2, top, bottom #-}

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

    prop_idf1, prop_idf2 :: d -> Bool
    prop_idf1 a   = ljoin a top == top
    prop_idf2 a = lmeet a bottom  == bottom

    prop_compSym :: d -> d -> Bool
    prop_compSym a b = lcompare a b == reverse4 (lcompare b a)


-- from wiki :
--An algebraic structure (L, join, jmeet), consisting of a set L
-- and two binary operations join (join), and jmeet (meet), on L
--is a lattice if the following axiomatic identities hold for all elements a, b, c of L.
--
--Commutative laws
--a join b = b join a,
--a jmeet b = b jmeet a.
--    	
--Associative laws
--a join (b join c) = (a join b) join c,
--a jmeet (b jmeet c) = (a jmeet b) jmeet c.
--    	
--Absorption laws
--a join (a jmeet b) = a,
--a jmeet (a join b) = a.
--The following two identities are also usually regarded as axioms,
-- even though they follow from the two absorption laws taken together.

--
--Idempotent laws
--a join a = a,
--a jmeet a = a.
--These axioms assert that both (L, join) and (L, jmeet) are semilattices.
-- The absorption laws, the only axioms above in which both meet
-- and join appear, distinguish a lattice from an arbitrary pair
-- of semilattices and assure that the two semilattices interact
-- appropriately. In particular, each semilattice is the dual of the other.
--
--Bounded lattice
--A bounded lattice is an algebraic structure of the form
-- (L, join, jmeet, 0, 1) such that
-- (L, join, jmeet) is a lattice,
-- 0 (the lattice's bottom) is the identity element
-- for the join operation join,
-- and 1 (the lattice's top) is the identity element
-- for the meet operation jmeet.
--
--Identity laws
--a join 0 = a,
--a && 1 = a.
--See semilattice for further details.






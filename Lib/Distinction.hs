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
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveAnyClass     #-}

module Distinction (Lattice (..), LatticeTests(..)
    , DistPaper
    , DistValue (..), dv2pair, pair2dv
    , B4val (..), PartialRel(..)
    , physObj'
    , human', stuff'
    )
    where

import Test.Framework
import Uniform.Strings
import Uniform.Error
--import qualified Data.PartialOrd as PO
--import GHC.Generic

import Belnap


data DistPaper = PhysObj | Human | Liquid | Edible | Dairy
            | Tool | Cuttlery | Fermented
                deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Arbitrary DistPaper where
    arbitrary = arbitraryBoundedEnum

data DistValue = DV {d :: DistPaper, v:: B4val} | DVtop | DVbot
            deriving (Show, Read, Eq, Ord)
            -- only True4 and Fals4 should occur, rest is always mapped to DVtop or DVbot?

--instance Arbitrary (DistPaper, DistValue) where

instance Arbitrary DistValue where
--    arbitrary = arbitrary -- map pair2dv arbitrary  -- (arbitrary, arbitrary)

-- these are raw distinction values, not taxa
physObj' = DV PhysObj True4
human' = DV Human True4
stuff' = DV Human False4
top' = DVtop
bot' = DVbot

dv2pair (DV d v) = (d,v)
pair2dv (d,v) = DV d v

instance Lattice DistValue where
--    lsub DVbot _ = True
--    lsub _ DVtop = True
--    lsub (DV d1 v1) (DV d2 v2) = if d1 == d2 then partialEQ v1 v2 else False
    lsub a b = case lcompare a b of
                    PLT -> True
                    _ -> False

    lcompare (DV d1 v1) (DV d2 v2) = if d1 == d2 then lcompare v1 v2 else INC

instance LatticeTests DistValue

test_1 = assertEqual (lsub human' human')  False
test_2 = assertEqual (lsub human' top') True
test_3 = assertEqual (lsub human' stuff') False

prop_compSymx :: DistValue -> DistValue -> Bool
prop_compSymx = prop_compSym

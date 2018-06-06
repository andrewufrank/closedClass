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

module Belnap (B4val (..)
    , htf_Belnap_thisModulesTests
--    , partialEQ
    , PartialRel (..)
    , Lattice (..), LatticeTests (..)
    )
    where

import Test.Framework
import Uniform.Strings
import Uniform.Error
import Lattice

data B4val = Both4 | True4 | False4 |  None4  deriving (Eq, Show, Read, Ord, Enum, Bounded)
-- | the four logical values of Belnaps 4 valued logic B4

        -- affirmativ, reject = negative, indiffirent = not apply, contradiction
        -- ord used for standard operations, eg. sort

instance Lattice B4val where
    top = None4    -- no knowledge
    bottom = Both4  -- contradiction
    lmeet2 a b = if a==b then a else bottom
    ljoin2 a b = if a==b then a else top

    lcompare2 a b = INC

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

instance  Arbitrary B4val where
    arbitrary = arbitraryBoundedEnum



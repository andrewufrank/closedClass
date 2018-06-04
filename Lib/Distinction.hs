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

module Distinction (Distinctions (..)
    , DistPaper
    , DistValue (..), dv2pair, pair2dv
    , BelnapLogic (..), PartialRel(..)
    , physObj'
    , human', stuff'
    )
    where

import Test.Framework
import Uniform.Strings
import Uniform.Error
--import GHC.Generic

import Belnap

class Distinctions d where
    sub, super :: d -> d -> Bool
--    order :: d -> d -> Ordering  -- use compare from Ord
    join, meet :: d -> d -> d

data DistPaper = PhysObj | Human | Liquid | Edible | Dairy
            | Tool | Cuttlery | Fermented
                deriving (Show, Read, Eq, Ord, Enum, Bounded)

data DistValue = DV {d :: DistPaper, v:: BelnapLogic} | DVtop | DVbot
            deriving (Show, Read, Eq, Ord)
            -- only Aff and Rej should occur, rest is always mapped to DVtop or DVbot?

-- these are raw distinction values, not taxa
physObj' = DV PhysObj Aff
human' = DV Human Aff
stuff' = DV Human Rej
top' = DVtop
bot' = DVbot

dv2pair (DV d v) = (d,v)
pair2dv (d,v) = DV d v

instance Distinctions DistValue where
    sub DVbot _ = True
    sub _ DVtop = True
    sub (DV d1 v1) (DV d2 v2) = if d1 == d2 then partialEQ v1 v2 else False


test_1 = assertEqual (sub human' human')  False
test_2 = assertEqual (sub human' top') True
test_3 = assertEqual (sub human' stuff') False


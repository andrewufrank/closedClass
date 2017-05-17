-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE DataKinds    #-}

module Peano
    where


import           Test.Framework
import Uniform.Strings

import Data.HList.FakePrelude  hiding (HLt, hLt)

-- | The data type to be lifted to the type level
data HNat = HZero | HSucc HNat


hZero :: Proxy HZero; hZero = Proxy
hSucc :: Proxy (n :: HNat) -> Proxy (HSucc n); hSucc _ = Proxy
hPred :: Proxy (HSucc n) -> Proxy n; hPred _ = Proxy

-- | Less than

type family HLt (x :: HNat) (y :: HNat) :: Bool

type instance HLt HZero HZero          = False
type instance HLt HZero (HSucc n)      = True
type instance HLt (HSucc n) HZero      = False
type instance HLt (HSucc n) (HSucc n') = HLt  n n'

hLt :: Proxy x -> Proxy y -> Proxy (HLt x y)
hLt _ _ = Proxy

--res :: _ = hLt one two

-- | @HDiv2 x@ behaves like @x `div` 2@
type family HDiv2x (x :: HNat) :: HNat
type instance HDiv2x HZero = HZero
type instance HDiv2x (HSucc HZero) = HZero
type instance HDiv2x (HSucc (HSucc a)) = HSucc (HDiv2 a)


--test_div2 = assertEqual one (HDiv2x two)

--class (HNat a,HNat b) => HAdd a b c | a b -> c where
--	hAdd :: a -> b -> c
--instance HAdd HZero HZero HZero where
--	hAdd _ _ = hZero
--instance HNat b => HAdd HZero (HSucc b) (HSucc b) where
--	hAdd _ b = b
--instance HNat a => HAdd (HSucc a) HZero (HSucc a) where
--	hAdd a _ = a
--instance (HNat (HSucc a),HNat (HSucc b),HNat c,HAdd a b c)
--	=> HAdd (HSucc a) (HSucc b) (HSucc (HSucc c)) where
--	hAdd _ _ = hSucc $ hSucc $ hAdd (undefined::a) (undefined::b)

--data Zero
--
--data Succ a
--
--class Add a b ab
--
--instance Add Zero b b
--instance (Add a b ab) => Add (Succ a) b (Succ ab)
--
--zero = undefined :: Zero
--one = undefined :: Succ zero
--two = add one one
----two =
----one = "1" :: Succ (Zero)
---- show produces the "xx"
----test_1 = assertEqual 7 (lengthChar $ op1 d1)
----test_2 = assertEqual 6 (lengthChar $ op1 d2)





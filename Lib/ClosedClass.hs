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
module ClosedClass -- (closedMain)
    where


import           Test.Framework
import Uniform.Strings

closedMain :: IO ()
closedMain = do
    return ()

data Data1 = Data1 {s :: String}
data Data2 ft = Data2 {f :: ft}
data Data3 = Data3 {i :: Int}

type family IsA t where
-- only the following two types are acceptable
    IsA (Data1) = String
    IsA (Data2 f) = Text

class X p where
    op1 :: p -> IsA p      -- show
--    op2 :: A t -> A t -> A t  -- add

instance  X (Data1 ) where
    op1 (Data1 s) = show s

instance Show f =>  X (Data2 f) where
    op1 (Data2 s) = s2t $ show s

--instance  X (Data3) where    -- does not compile
--    op1 (Data3 s) =   show s

d1 = Data1 "eines"
d2 = Data2 "zwei"
d2a = Data2 'a'
d3 = Data3 3

-- show produces the "xx" form, thus 2 char longer than expected
test_1 = assertEqual 7 (getLength d1)
test_2 = assertEqual 6 (getLength d2)
test_3 = assertEqual 3 (getLength d2a)


getLength :: (CharChains (IsA a), X a) =>  a -> Int
getLength a = lengthChar $ op1 a


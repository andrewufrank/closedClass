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
data Data2 = Data2 {f :: Text}
data Data3 = Data3 {i :: Int}

type family IsA t where
--    op1 :: A t -> String      -- show
--    op2 :: A t -> A t -> A t  -- add
-- only the following two types are acceptable
    IsA (Data1) = String
    IsA (Data2) = Text

class X p where
    op1 :: p -> IsA p      -- show
--    op2 :: A t -> A t -> A t  -- add

instance  X (Data1 ) where
    op1 (Data1 s) = show s

instance  X (Data2 ) where
    op1 (Data2 s) = s2t $ show s

--instance  X (Data3) where    -- does not compile
--    op1 (Data3 s) =   show s

d1 = Data1 "eines"
d2 = Data2 "zwei"
d3 = Data3 3

-- show produces the "xx"
test_1 = assertEqual 7 (lengthChar $ op1 d1)
test_2 = assertEqual 6 (lengthChar $ op1 d2)





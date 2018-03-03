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

module Lib.Tutorial1_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import           Uniform.Strings
import Uniform.FileIO
--import qualified Data.ByteString.Lazy as B

import Lib.Tutorial1
--import Data.String


-- show produces the "xx"
test_1 = do
    res0 <- runErr $ do
        let fn = makeRelFile "tutorial1.json"
        putIOwords ["tutorial json decode:", showT fn]
        f <- readFile2  fn
        putIOwords ["json input:", take' 100 . showT $ f]
        let r = decodePerson f  -- :: Maybe [Doc1]
        putIOwords ["decoded:", showT r]
        return r
    assertEqual res (show res0)

test_2 = assertEqual 6 6

res = "Right (Just [Person {firstName = \"Daniel\", lastName = \"D\\237az\", \
    \age = 24, likesPizza = True},Person {firstName = \"Rose\", \
    \lastName = \"Red\", age = 39, likesPizza = False},Person \
    \{firstName = \"John\", lastName = \"Doe\", age = 45, likesPizza = False},\
    \Person {firstName = \"Vladimir\", lastName = \"Vygodsky\", age = 27, \
    \likesPizza = False},Person {firstName = \"Foo\", lastName = \"Bar\", \
    \age = 32, likesPizza = True},Person {firstName = \"Mar\\237a\", \
    \lastName = \"Delaoh\", age = 52, likesPizza = False},\
    \Person {firstName = \"Victoria\", lastName = \"Haskell\", age = 23, \
    \likesPizza = True},Person {firstName = \"Fran\\231ois\", \
    \lastName = \"Beaulieu\", age = 42, likesPizza = False},\
    \Person {firstName = \"Amalie\", lastName = \"Baumann\", age = 28, \
    \likesPizza = True},Person {firstName = \"Rachel\", lastName = \"Scott\", \
    \age = 23, likesPizza = True}])"




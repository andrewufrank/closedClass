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
import qualified Data.ByteString.Lazy as B

import Lib.Tutorial1
import Data.String

jsonFile :: FilePath
jsonFile = "tutorial1.json"


-- show produces the "xx"
test_1 = do
    f <- readBSlazy  "tutorial1.json"
    let r = decodePerson f :: Maybe [Person]
    putIOwords ["decoded", showT r]
    assertEqual res (show r)
test_2 = assertEqual 6 6

res = "Just [Person {firstName = \"Daniel\", lastName = \"D\\237az\", \
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
    \age = 23, likesPizza = True}]"




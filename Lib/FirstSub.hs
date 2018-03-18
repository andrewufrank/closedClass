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

module FirstSub
    where

import           Test.Framework
import Uniform.Strings
import Uniform.Error
import Uniform.FileIO
import Conllu.IO
firstMain :: ErrIO ()
firstMain = do
    putIOwords ["read a udfeat file and parse"]
    in1 <- readFile2 (makeRelFile "short1ud.txt")
    putIOwords ["read short1ud.txt", in1]
    res1 <- callIO $  readConllu "veryshort1ud.txt"
    putIOwords ["result from readConllu", showT res1]

    return ()

-----------------------------------------------------------------------------
--
-- Module      :  testing Monoid deriving
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module FirstSub
    where

import Data.Text as T
import Data.Text.IO as T

import Control.Monad.Error  -- is monads-tf

import Uniform.Zero

class (Monoid l, Zeros (LF l)) => ListForms l   where
    type LF l
    prependOne  :: (LF l) -> l   -> l
    appendOne :: l   -> (LF l) -> l
    mkOne :: (LF l) -> l
    appendTwo  :: l  -> l   -> l

    prependOne a la = appendTwo  (mkOne a) la
    appendOne la a = appendTwo la (mkOne a)
    appendTwo = (<>)

firstMain :: IO ()
firstMain = return ()

putIOwords ::  [Text] -> IO()
putIOwords = T.putStrLn . T.unwords


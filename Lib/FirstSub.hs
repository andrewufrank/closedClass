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
{-# LANGUAGE StandaloneDeriving
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , DeriveAnyClass
      #-}

module FirstSub
    where

import Data.Text as T
import Data.Text.IO as T
import GHC.Generics

import Control.Monad.Error  -- is monads-tf

import Uniform.Zero

class ListForms l   where
    type LF l
    prependOne  :: (LF l) -> l   -> l
    appendOne :: l   -> (LF l) -> l
    mkOne :: (LF l) -> l
    unMakeOne :: l -> (LF l)  -- succeds only for singleton, not for export
    appendTwo  :: l  -> l   -> l

    prependOne a la = appendTwo  (mkOne a) la
    appendOne la a = appendTwo la (mkOne a)
--    appendTwo = (<>)
    {-# Minimal appendTwo, mkOne #-}

-- a server URI (not including the port, but absolute)
newtype T1 = T1 {unT1 :: Text}
                deriving (Show, Read, Eq, Ord, Generic, Zeros  -- , Semigroup, Monoid
--                        , ListForms
                    )


instance Zeros Text where zero = ""

instance ListForms Text where
    type LF Text = Char
    mkOne = T.singleton
    unMakeOne = T.head
    appendTwo = T.append
--deriving
instance   ListForms T1
    where
    type LF T1 = Text
    mkOne = T1
    unMakeOne = unT1
    appendTwo a b = mkOne $ appendTwo (unMakeOne a)  (unMakeOne b)

firstMain :: IO ()
firstMain = return ()

putIOwords ::  [Text] -> IO()
putIOwords = T.putStrLn . T.unwords


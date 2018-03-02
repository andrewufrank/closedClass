-----------------------------------------------------------------------------
--
-- Module      :  checking the code for reading nested json from blog
-- http://blog.gnclmorais.com/aeson-and-nested-json
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# LANGUAGE DeriveGeneric #-}

module Lib.Tutorial1  -- (openMain, htf_thisModuelsTests)
     where


import           Uniform.Strings

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B

tutorial1Main :: IO ()
tutorial1Main = do
    f <- readBSlazy  "tutorial1.json"
    let r = decode f  :: Maybe [Person]
    putIOwords ["decoded", showT r]
    return ()

--decode :: FromJSON a => ByteString -> Maybe a
--encode :: ToJSON a => a -> ByteString
--eitherDecode :: FromJSON a => ByteString -> Either String a

decodePerson :: B.ByteString -> Maybe [Person]
decodePerson = decode

data Person =
  Person { firstName  :: !Text
         , lastName   :: !Text
         , age        :: Int
         , likesPizza :: Bool
           } deriving (Show, Generic, Eq)

instance FromJSON Person
instance ToJSON Person


--jsonFile :: FilePath
--jsonFile = "tutorial1.json"
--
readBSlazy :: FilePath ->  IO B.ByteString
readBSlazy  =  B.readFile






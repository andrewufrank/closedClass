-----------------------------------------------------------------------------
--
-- Module      :   main for tests
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- {-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- {-# LANGUAGE DeriveGeneric #-}



module Main
where      -- must have Main (main) or Main where


import           Lib.DerivingMinimalExample

main :: IO ()
main = do
    deriveTest2
        -- insert here modules in IO




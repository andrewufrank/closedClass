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
{-# LANGUAGE DefaultSignatures
    , DeriveGeneric
    , TypeOperators
         #-}
module Lib.DerivingExample
    where


import           Test.Framework
import Data.Text
--import Uniform.Strings
import GHC.Generics
import Data.Monoid

deriveTest :: IO ()
deriveTest = return ()

class   Zeros z where
    zero :: z
--    default zero :: (Generic z, Gzero (Rep z)) => z
--    zero = gzero (from z)
--
--class Gzero f  where
--    gzero :: f a -> a
--instance Gzero U1 where
--    gzero U1 = a

--    zero = gzero
--    isZero  :: Eq z =>  z -> Bool
--    isZero z = zero == z
--    notZero :: Eq z =>  z -> Bool
--    notZero = not.isZero
--    zero = error   ("Zeros for this type not instantiated ")
--                ++ typeinfo)
--            where typeinfo = show $ typeOf zero
    -- gives a difficult to track error, because the type is not known



--class (Ones z) =>  Ones (f z) where
--    one :: f z
--    default one :: f z
--    one = gone (from z1)
--
--class Gzero f  where
--    gzero :: f a -> a
--instance Gzero U1 where
--    gzero U1 = a


instance Zeros Char where zero = ' '
--instance (Zeros a, Eq a) => Zeros [a] where zero = []
instance Zeros () where zero = ()
--instance Zeros String where zero = ""
--instance Zeros Strings where zero = []
instance Zeros Int where zero = 0
instance Zeros [a] where zero = []
instance (Zeros a, Zeros b) => Zeros (a,b) where zero = (zero, zero)
instance (Zeros a, Zeros b, Zeros c) => Zeros (a,b,c) where
     zero = (zero, zero, zero)
instance (Zeros a, Zeros b, Zeros c, Zeros d) => Zeros (a,b,c,d) where
     zero = (zero, zero, zero, zero)
instance (Zeros a, Zeros b, Zeros c, Zeros d, Zeros e)
         => Zeros (a,b,c, d, e) where
     zero = (zero, zero, zero, zero, zero)

instance Zeros Text where zero = (""::Text)
instance Zeros (Maybe a) where zero = Nothing -- added



{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE IncoherentInstances #-}

module HaskellObjetoso where
import Prelude hiding ((.))

(·) f1 f2 = (\x -> f1 $ f2 x)
(<<) f1 f2 = (\x -> f1 (f2 x) x)
nullPionterError = error "Null pointer exception"


data Object object = 
	Instance{ 
	dataObject :: object 
	} |
	Null deriving (Eq)
	
instance Show a => Show (Object a) where
	show (Instance dataObject) = "#Object \n  " ++ show dataObject
	show Null = "#Null"

instance Functor Object where
	fmap f (Instance dataObject) = Instance $ f dataObject
	fmap _ Null = nullPionterError

-- (.) :: Object a -> (Object a -> b) -> (Object b)
-- (.) object f = Instance $ f object
(!) :: Object a -> (a -> b) -> (Object b)
(!) = flip fmap
	
	
isNull Null = True
isNull _ = False

type ObjectFunction a b = (Object a) -> b
	
class FunctionClass f where
	(.) :: (Object a) -> (f a b) -> (Object b)
	
instance FunctionClass (->) where
	(.) = flip fmap

instance FunctionClass ObjectFunction where
	(.) object f = Null
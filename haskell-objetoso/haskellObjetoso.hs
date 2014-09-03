{-# LANGUAGE FlexibleInstances #-}

module HaskellObjetoso where
import Prelude hiding ((.))

(·) f1 f2 = (\x -> f1 $ f2 x)
(<<) f1 f2 = (\x -> f1 (f2 x) x)
nullPionterError = error "Null pointer exception"
crashError = error "Crash type exception"

data Object dataO = Instance dataO | Null deriving (Eq, Ord, Read)
	
	
data Method a b = Method { method :: (Object a) -> (Object b) }

instance Show (Method a b) where
	show _ = "#Metodo"
	
class MethodClass f where
	(.) :: (Object a) -> (f a b) -> (Object b)
instance MethodClass (->) where
	(.) = flip fmap
instance MethodClass Method where
	(.) object (Method f) = f object
		
isNull Null = True
isNull _ = False



instance Show a => Show (Object a) where
	show (Instance dataObject) = "#Object \n  " ++ show dataObject
	show Null = "#Null"

instance Functor Object where
	fmap f (Instance a) = Instance $ f a
	fmap _ Null = nullPionterError

instance Monad Object where
	(>>=) (Instance a) f = f a
	return = Instance
	(>>) (Instance a) (Instance b) = crashError
	(>>) object Null = nullPionterError
	(>>) Null object = object

instance Num a => Num (Object a) where
	(+) (Instance a) (Instance b) = Instance $ a + b
	(-) (Instance a) (Instance b) = Instance $ a - b
	(*) (Instance a) (Instance b) = Instance $ a * b
	negate (Instance a) = Instance $ negate a
	abs (Instance a) = Instance $ abs a
	signum (Instance a) = Instance $ signum a
	fromInteger i = Instance $ fromInteger i 
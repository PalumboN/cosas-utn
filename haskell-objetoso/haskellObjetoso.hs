{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
module HaskellObjetoso where
import Prelude hiding ((.))


(·) f1 f2 = (\x -> f1 $ f2 x)
(<<) f1 f2 = (\x -> f1 (f2 x) x)

data Object object = New { dataObject :: object } deriving (Eq)

class ObjectClass dataObject where
	getMethods :: [(Object dataObject) -> (Object a)]

instance Show a => Show (Object a) where
	show (New dataObject) = "#Object \n  " ++ show dataObject

-- (.) :: (Object a) -> (ObjectFunction a b) -> (Object b)
(.) (New dataObject) f = f dataObject

data ObjectFunction a b = OF ((Object a) -> b) | SF (a -> b)

instance Show (ObjectFunction a b) where
	show (OF f) = "OF -"
	show (SF f) = "SF -"

apply :: (ObjectFunction a b) -> (Object a) -> b
apply (OF f) object = f object
apply (SF f) (New a) = f a

transform f = SF f

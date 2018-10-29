{-# LANGUAGE MagicHash, ExplicitForAll, PolyKinds, TypeInType #-}

import GHC.Prim (Int#, (+#), TYPE)
import GHC.Types (Int(I#), RuntimeRep)

liftedFunc :: Int -> Int -> Int
liftedFunc x y
  | x < y = x + y
  | otherwise = myError "x < y"

unliftedFunc :: Int# -> Int# -> Int#
unliftedFunc x y
  | (I# x) < (I# y) = x +# y
  | otherwise = myError "x < y"

myError
  :: forall (l :: RuntimeRep) (a :: TYPE l).
     String -> a
myError s = error ("Error: " ++ s)

main :: IO ()
main = do
  print $ liftedFunc 1 2
  print $ I# (unliftedFunc 1# 2#)
  print $ I# (unliftedFunc 2# 1#) -- error
  print $ liftedFunc 2 1 -- error

{-# LANGUAGE MagicHash, ExplicitForAll, PolyKinds #-}

import GHC.Prim (Int#, (+#))
import GHC.Types (Int(I#))

liftedFunc :: Int -> Int -> Int
liftedFunc x y
  | x < y = x + y
  | otherwise = myError "x < y"

unliftedFunc :: Int# -> Int# -> Int#
unliftedFunc x y
  | (I# x) < (I# y) = x +# y
  | otherwise = myError "x < y"

myError :: String -> a
myError s = error ("Error: " ++ s)

unliftedValue :: Int#
unliftedValue = 0#

main :: IO ()
main = do
  print $ liftedFunc 1 2
  print $ I# (unliftedFunc 1# 2#)
  print $ I# (unliftedFunc 2# 1#) -- error
  print $ liftedFunc 2 1 -- error

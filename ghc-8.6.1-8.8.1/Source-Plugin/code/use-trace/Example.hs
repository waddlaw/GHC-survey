module Example where

f1 :: Int
f1 = f2

f2 :: Int
f2 = f3

f3 :: Int
f3 = error "f3"

f4 :: HasCallStack => Int
f4 = f1

f5 :: Int
f5 = f6
  where f6 = f1
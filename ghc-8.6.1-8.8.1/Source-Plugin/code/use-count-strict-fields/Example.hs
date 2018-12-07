{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
module Example where

data MyData1   = MyDataCon1
data MyData2 a = MyDataCon2 a
data MyData3 b = MyDataCon3 !b
data MyData4   = MyDataCon4 Int
data MyData5   = MyDataCon5 !Int Int Char Bool !(MyData3 Int)
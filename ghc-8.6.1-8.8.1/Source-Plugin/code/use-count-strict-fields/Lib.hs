{-# LANGUAGE BangPatterns #-}
module Lib where

data MyLib1 = MyLib1
data MyLib2 = MyLib2 (Maybe Int) !(Maybe Int) !(Maybe Int)
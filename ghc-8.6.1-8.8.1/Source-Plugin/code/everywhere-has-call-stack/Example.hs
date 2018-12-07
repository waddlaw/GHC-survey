{-# OPTIONS_GHC -fplugin EverywhereHasCallStack #-} 
module Example where

-- -- HsQualTy
-- exHsQualTy :: HasCallStack => Int
-- exHsQualTy = 5

-- -- HsTyVar
-- exHsTyVar :: Int
-- exHsTyVar = 6

-- -- HsFunTy
-- exHsFunTy :: Int -> Int -> Int
-- exHsFunTy x y = 7

-- -- HsListTy
-- exHsListTy :: [Int]
-- exHsListTy = [8]

f1 :: Int
f1 = f2

f2 :: Int
f2 = f3

f3 :: Int
f3 = error "f3"
{-# OPTIONS_GHC -fplugin AddDecl #-} 
{-# LANGUAGE TemplateHaskell #-}
module Example where

data MyData = MyData

a = ()

$(return [])

b :: Int
b = g + 2
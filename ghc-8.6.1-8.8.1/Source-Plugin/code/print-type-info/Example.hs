{-# OPTIONS_GHC -fplugin BasicPluginSimple #-} 
{-# LANGUAGE TemplateHaskell #-}
module Example where

data MyData = MyData

a = ()

$(return [])
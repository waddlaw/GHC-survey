{-# OPTIONS_GHC -fplugin BasicPluginSimple #-} 
{-# LANGUAGE TemplateHaskell #-}
module Example where

a = ()

$(return [])
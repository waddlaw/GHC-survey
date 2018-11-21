{-# OPTIONS_GHC -fplugin SourcePlugin #-} 
{-# LANGUAGE TemplateHaskell #-}
module A where

a = ()

$(return [])
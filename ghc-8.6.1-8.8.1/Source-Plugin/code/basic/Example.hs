{-# OPTIONS_GHC -fplugin BasicPlugin #-} 
{-# LANGUAGE TemplateHaskell #-}
module Example where

a = ()

$(return [])
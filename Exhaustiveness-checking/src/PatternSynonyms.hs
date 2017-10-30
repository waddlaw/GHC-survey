{-# LANGUAGE PatternSynonyms #-}
module Patterns where

data ErrorCall = ErrorCallWithLocation String String deriving (Eq, Ord)

pattern ErrorCall :: String -> ErrorCall
pattern ErrorCall err = ErrorCallWithLocation err ""

getMsg :: ErrorCall -> String
getMsg (ErrorCall y) = y

getMsg' :: ErrorCall -> String
getMsg' (ErrorCallWithLocation y _) = y

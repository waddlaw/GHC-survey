{-# LANGUAGE MagicHash #-}
import GHC.Prim ((+#))
import GHC.Types (Int(I#))
import GHC.CString (unpackCString#)

main = do
  putStrLn $ show $ I# (1# +# 5#)
  putStrLn $ show $ unpackCString# "foo"#

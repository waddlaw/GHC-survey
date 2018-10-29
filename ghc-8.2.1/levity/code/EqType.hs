module Main where

import GHC
import GHC.Paths (libdir)
import HsImpExp (simpleImportDecl)
import MonadUtils (liftIO)
import TcType (tcEqType, toTcType)
import Type (eqType)

main :: IO ()
main =
  runGhc (Just libdir) $ do
    getSessionDynFlags >>= setSessionDynFlags
    setContext [IIDecl $ simpleImportDecl $ mkModuleName "Prelude"]
    t1 <- exprType "True"
    t2 <- exprType "Just True"
    (_, k1) <- typeKind False "Eq Bool"
    (_, k2) <- typeKind False "Maybe Bool"
    -- type equality
    liftIO $ print $ eqType t1 t1 -- True
    liftIO $ print $ eqType t1 t2 -- False
    liftIO $ print $ tcEqType t1 t1 -- True
    liftIO $ print $ tcEqType t1 t2 -- False
    -- kind equality
    liftIO $ print $ eqType k1 k1 -- True
    liftIO $ print $ eqType k1 k2 -- True
    liftIO $ print $ tcEqType k1 k1 -- True
    liftIO $ print $ tcEqType k1 k2 -- False

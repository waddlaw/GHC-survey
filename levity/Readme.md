# GHC Developers Trac
## wiki
- [NoSubKinds](https://ghc.haskell.org/trac/ghc/wiki/NoSubKinds)
    - [日本語訳](/levity/NoSubKinds.md)
- [The data type Type and its friends](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/TypeType)
- [Unlifted data types](https://ghc.haskell.org/trac/ghc/wiki/UnliftedDataTypes)
- [Unboxed sum types](https://ghc.haskell.org/trac/ghc/wiki/UnpackedSumTypes)
- [DependentHaskell](https://ghc.haskell.org/trac/ghc/wiki/DependentHaskell)
- [DependentHaskell/Phase1](https://ghc.haskell.org/trac/ghc/wiki/DependentHaskell/Phase1)

## ticket
- [Levity polymorphism](https://ghc.haskell.org/trac/ghc/wiki/LevityPolymorphism)
- [Kind polymorphism and unboxed types: bad things are happening](https://ghc.haskell.org/trac/ghc/ticket/11471)
- [($) returning kind # no longer type checks](https://ghc.haskell.org/trac/ghc/ticket/8739)

# GHC Code
- [GHC.Types](https://github.com/ghc/ghc/blob/master/libraries/ghc-prim/GHC/Types.hs)
- [CoreSyn](https://github.com/ghc/ghc/blob/master/compiler/coreSyn/CoreSyn.hs)
- [Type](https://github.com/ghc/ghc/blob/master/compiler/types/Type.hs)
- [Kind](https://github.com/ghc/ghc/blob/master/compiler/types/Kind.hs)
- [TcType](https://github.com/ghc/ghc/blob/master/compiler/typecheck/TcType.hs)

# その他
- [What is Levity polymorphism](https://stackoverflow.com/questions/35318562/what-is-levity-polymorphism/35320729#35320729)
- [Richard Eisenberg - Levity Polymorphism (HiW)](http://ezyang.tumblr.com/post/127984263867/richard-eisenberg-levity-polymorphism-hiw)
- Levity Polymorphism in Dependent Haskell. Presented at Haskell Implementors' Workshop
    - [slide](http://cs.brynmawr.edu/~rae/talks/2015/hiw-levity-polymorphism.pdf)
    - [youtube](https://www.youtube.com/watch?v=bDdkeKr9vVw)
- [Richard A. Eisenberg - Levity Polymorphism](https://www.youtube.com/watch?v=lSJwXZ7vWBw)
- [9.2. Unboxed types and primitive operations](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#unboxed-types-and-primitive-operations)
- [9.2.1. Unboxed types](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#unboxed-types)
- [What are lifted and unlifted product types in Haskell?](https://stackoverflow.com/questions/39985296/what-are-lifted-and-unlifted-product-types-in-haskell)
- [Richard A. Eisenberg](http://cs.brynmawr.edu/~rae/index.html)

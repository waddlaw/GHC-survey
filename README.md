# GHC-survey

- [haskell/ghcup](https://github.com/haskell/ghcup) を使うと便利
- [ghc/ghc](https://github.com/ghc/ghc)

バージョン | status | release note
----|----|----
8.8.1 | [status](https://ghc.haskell.org/trac/ghc/wiki/Status/GHC-8.8.1) |
8.6.1 | [status](https://ghc.haskell.org/trac/ghc/wiki/Status/GHC-8.6.1) | [note](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/8.6.1-notes.html)
8.2.1 | [status](https://ghc.haskell.org/trac/ghc/wiki/Status/GHC-8.2.1) | [note](https://downloads.haskell.org/~ghc/master/users-guide/8.2.1-notes.html)

## 発表内容

### GHC-8.6.1, GHC-8.8.1

発表内容 | 担当者 | 資料
----|----|----
Hadrian | @waddlaw
BlockArguments | @waddlaw
NumericUnderscores | @waddlaw
StarIsType | @waddlaw
Source Plugins | @waddlaw | [slide](https://gitpitch.com/waddlaw/GHC8.2.1-survey/slide-ghc-source-plugin#/)
GHCi :doc command | @waddlaw
As-pattern synonyms | @waddlaw
Valid Hole Fits | @Wataru
Quantified Class Constraints | 
DerivingVia | 

### GHC-8.2.1

発表内容 | 担当者 | 資料
----|----|----
Levity polymorphism | @waddlaw | [paper](/levity/levity-polymorphism.md), [note](/levity/note.md), [slide](https://gitpitch.com/waddlaw/GHC8.2.1-survey/slide-levity-polymorphism#/)
Exhaustiveness checking | @waddlaw | [slide](https://gitpitch.com/waddlaw/GHC8.2.1-survey/slide-pattern-synonyms#/) |
Indexed Typeable representations | @pythonissam |
Deriving | @pythonissam |
Backpack | @matsubara0507 |
Overloaded record fields | @matsubara0507 |
Unpacked sum types | |

## GHC の理解を深めるために

- [GHC Developer Wiki](https://ghc.haskell.org/trac/ghc/)
  - [The GHC reading list](https://ghc.haskell.org/trac/ghc/wiki/ReadingList)
  - [The GHC Commentary](https://ghc.haskell.org/trac/ghc/wiki/Commentary)
    - [Layout of important files and directories](https://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree)
    - [The Marvellous Module Structure of GHC](https://ghc.haskell.org/trac/ghc/wiki/Commentary/ModuleStructure)
    - [GHC Source Code Abbreviations](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Abbreviations)
    - [GHC Commentary: The Compiler](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler)
  - [Debugging](https://ghc.haskell.org/trac/ghc/wiki/Debugging)

- [A Haskell Implementation Reading List](http://www.stephendiehl.com/posts/essential_compilers.html)
- [Dive into GHC: Pipeline](http://www.stephendiehl.com/posts/ghc_01.html)
- [Dive into GHC: Intermediate Forms](http://www.stephendiehl.com/posts/ghc_02.html)
- [Dive into GHC: Targeting Core](http://www.stephendiehl.com/posts/ghc_03.html)

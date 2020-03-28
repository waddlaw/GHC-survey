# GHC-survey

- [haskell/ghcup](https://www.haskell.org/ghcup/) を使うと便利
- [ghc/ghc](https://github.com/ghc/ghc)

バージョン | status | release note
----|----|:----:
8.10.1 | [status][status-8101] | [note][note-881]
8.8.1 | [status][status-881] | [note][note-881]
8.6.1 | [status][status-861] | [note][note-861]
8.2.1 | [status][status-821] | [note][note-821]

[status-8101]: https://gitlab.haskell.org/ghc/ghc/-/wikis/status/ghc-8.10.1
[status-881]: https://gitlab.haskell.org/ghc/ghc/-/wikis/status/ghc-8.8.1
[status-861]: https://gitlab.haskell.org/ghc/ghc/-/wikis/status/ghc-8.6.1
[status-821]: https://gitlab.haskell.org/ghc/ghc/-/wikis/status/ghc-8.2.1

[note-8101]: https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/8.10.1-notes.html
[note-881]: https://downloads.haskell.org/~ghc/8.8.1/docs/html/users_guide/8.8.1-notes.html
[note-861]: https://downloads.haskell.org/~ghc/8.6.1/docs/html/users_guide/8.6.1-notes.html 
[note-821]: https://downloads.haskell.org/~ghc/8.6.1/docs/html/users_guide/8.2.1-notes.html 

## 発表内容

### GHC-8.6.1, GHC-8.8.1

発表内容 | 担当者 | 資料
----|----|----
Hadrian
BlockArguments
NumericUnderscores
StarIsType
Source Plugins | @waddlaw | [slide](https://gitpitch.com/waddlaw/GHC-survey/slide-ghc-source-plugin#/)
GHCi :doc command
As-pattern synonyms 
Valid Hole Fits | @Wataru | [slide](https://wataru86.github.io/slides/vhs/)
Quantified Class Constraints
DerivingVia

### GHC-8.2.1

発表内容 | 担当者 | 資料
----|----|----
Levity polymorphism | @waddlaw | [paper](/levity/levity-polymorphism.md), [note](/levity/note.md), [slide](https://gitpitch.com/waddlaw/GHC-survey/slide-levity-polymorphism#/)
Exhaustiveness checking | @waddlaw | [slide](https://gitpitch.com/waddlaw/GHC-survey/slide-pattern-synonyms#/) |
Indexed Typeable representations | @pythonissam |
Deriving | @pythonissam |
Backpack | @matsubara0507 | [slide](https://www.slideshare.net/noob00/haskell-backpack)
Overloaded record fields | @matsubara0507 |
Unpacked sum types | |

## GHC の理解を深めるために

- [GHC Developer Wiki](https://gitlab.haskell.org/ghc/ghc)
  - [The GHC reading list](https://gitlab.haskell.org/ghc/ghc/-/wikis/reading-list)
  - [The GHC Commentary](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary)
    - [Layout of important files and directories](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/source-tree)
    - [The Marvellous Module Structure of GHC](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/module-structure)
    - [GHC Source Code Abbreviations](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/abbreviations)
    - [GHC Commentary: The Compiler](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler)
  - [Debugging](https://gitlab.haskell.org/ghc/ghc/-/wikis/debugging)
- [The Glasgow Haskell Compiler a contributor's cheatsheet](https://ghc.dev/)
- [A Haskell Implementation Reading List](http://www.stephendiehl.com/posts/essential_compilers.html)
- [Dive into GHC: Pipeline](http://www.stephendiehl.com/posts/ghc_01.html)
- [Dive into GHC: Intermediate Forms](http://www.stephendiehl.com/posts/ghc_02.html)
- [Dive into GHC: Targeting Core](http://www.stephendiehl.com/posts/ghc_03.html)

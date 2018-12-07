# GHC Source Plugin

---

## はじめに

本資料では、GHC Source Plugin そのものについてはあまり説明しないため、必要に応じて下記の資料を参照してください。

- [Source Plugins - ICFP 2018](https://icfp18.sigplan.org/event/hiw-2018-papers-source-plugins)
- [Source plugins - GHC User Manual](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#source-plugins)
- [GHC Source Plugin 作ってみた - Qiita](https://qiita.com/waddlaw/items/65b57517f105fcbbe724)

---

## GHC Source Plugin とは？

## 従来の Source Plugin

図の赤色の領域が従来の Source Plugin です。

`Desugar` -> `Core to core` の間に差し込むことができます。

+++

### 問題点

- `Core` の知識が必要
- `Core to core` の最適化にしか使えなかった

### 実際に作られた plugin

- [strict-ghc-plugin](http://hackage.haskell.org/package/strict-ghc-plugin)
- [cse-ghc-plugin](http://hackage.haskell.org/package/cse-ghc-plugin)
# GHC Source Plugin

---

## はじめに

本資料は GHC に馴染みが無くても理解できるような構成になっています。

GHC Source Plugin のおもしろさを伝えたい！

---

## GHC Source Plugin とは？

![ICFP から図を引用](images/map.png)

+++

## 従来の Source Plugin

図の赤色の領域が従来の Source Plugin です。

`Desugar` -> `Core to core` の間に差し込むことができます。

+++

### 実際に作られた plugin

- [strict-ghc-plugin](http://hackage.haskell.org/package/strict-ghc-plugin)
- [cse-ghc-plugin](http://hackage.haskell.org/package/cse-ghc-plugin)

+++

### 全然流行らなかった！！！

+++

### 問題点

- `Core` の知識が必要
- `Core to core` の最適化にしか使えなかった
- 当時は [ghc](https://hackage.haskell.org/package/ghc-8.6.1) パッケージとか無かった

+++

### つまり

---

### 終わりに

興味を持った人は、以下の資料を読めば完全に一人でプラグインを作れるようになります！

- [Source Plugins - ICFP 2018](https://icfp18.sigplan.org/event/hiw-2018-papers-source-plugins)
- [Source plugins - GHC User Manual](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#source-plugins)
- [GHC Source Plugin 作ってみた - Qiita](https://qiita.com/waddlaw/items/65b57517f105fcbbe724)

---

### 作ってみよう!GHC SOURCE PLUGIN!!
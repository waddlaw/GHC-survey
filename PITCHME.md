@title[Exhaustiveness Checking]

# Exhaustiveness Checking

---

@title[Exhaustiveness Checking とは？]

## Exhaustiveness Checking とは 何か？

+++

```haskell
bad :: Bool -> Bool
bad True = undefined
```

実行してみましょう！

+++

何言ってるんですか？
警告出てないじゃないですか！

```bash
$ stack repl --resolver=ghc-8.0.2 bad.hs
[1 of 1] Compiling Main
Ok, 1 module loaded.
*Main>
```

なぜでしょう？

+++

```bash
$ stack repl --resolver=ghc-8.0.2 --ghc-options="-Wall" bad.hs
[1 of 1] Compiling Main
Ok, 1 module loaded.
*Main>
```

デフォルトでは警告が出ないので、ちゃんと `--ghc-options` を渡しましょう。



以下の言語拡張を利用している場合のパターン欠如に対して、警告を出すようになった。
- EmptyCase
- PatternSynonyms


---
@title[EmptyCase]
EmptyCase

---
@title[PatternSynonyms]

PatternSynonyms1

+++

PatternSynonyms2

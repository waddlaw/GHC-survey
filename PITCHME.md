@title[Exhaustiveness Checking]

# Exhaustiveness Checking

---

@title[Exhaustiveness Checking とは？]

## Exhaustiveness Checking <br>とは 何か？

+++

実行してみましょう！

```haskell
bad :: Bool -> Bool
bad True = undefined
```

```bash
$ stack repl --resolver=ghc-8.0.2 bad.hs
[1 of 1] Compiling Main
Ok, 1 module loaded.
*Main>
```

何言ってるんですか？警告出てないじゃないですか！

Question: なぜ警告が出ないのでしょうか？ 

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

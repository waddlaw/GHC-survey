{-# LANGUAGE PatternSynonyms #-}

type Answer = Bool

pattern Yes :: Answer
pattern Yes = True
pattern No :: Answer
pattern No = False

showAnswer :: Answer -> String
showAnswer ans =
  if ans
    then "YEEEEES"
    else "NOOOOOO"

pattern Head :: a -> [a]
pattern Head x <- x:_ -- _ を含む物は = では定義できない

showHead :: [a] -> String
showHead (Head _) = undefined
showHead _  = "No head!"

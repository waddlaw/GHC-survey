f :: Bool
f = True

g
  :: Eq a
  => a -> a -> Bool
g a1 a2 = a1 == a2

h
  :: (Eq a, Eq b)
  => a -> a -> b -> b -> Bool
h a1 a2 b1 b2 = a1 == a2 && b1 == b2

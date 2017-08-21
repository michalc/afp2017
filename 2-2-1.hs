-- Exercise 2.2.1  Write a tail-recursive variant of splitleft.

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving(Show)

splitleft :: Tree a -> (a, Maybe (Tree a))
splitleft (Leaf a)   = (a, Nothing)
splitleft (Node l r) = case splitleft l of
  (a, Nothing) -> (a, Just r)
  (a, Just l') -> (a, Just (Node l' r))

splitleft' :: Tree a -> (a, Maybe (Tree a))
splitleft' t = splitleftCPS t id

splitleftCPS :: Tree a -> ((a, Maybe (Tree a)) -> r) -> r
splitleftCPS (Leaf a)   f = f (a, Nothing)
splitleftCPS (Node l r) f = splitleftCPS l $ \res ->
  case res of
    (a, Nothing) -> f (a, Just r)
    (a, Just l') -> f (a, Just (Node l' r))

main :: IO()
main = putStrLn $ show $ splitleft' $ Node (Node (Leaf "a" ) (Leaf "b")) (Leaf "c")

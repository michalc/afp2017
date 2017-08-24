data Tree = Bin Tree Integer Tree | Leaf deriving(Show, Eq, Ord)

input :: Tree
input = Bin (Leaf) 99 (Bin (Bin (Leaf) 99 (Leaf)) 99 (Leaf))

desired :: Tree
desired = Bin (Leaf) 1 (Bin (Bin (Leaf) 3 (Leaf)) 2 (Leaf))

main :: IO()
main =  putStrLn ("Input: " ++ (show input))
     >> putStrLn ("Output: " ++ (show $ bfl input))
     >> putStrLn ("Desired: " ++ (show desired))
     >> putStrLn ("Is desired? " ++ show (bfl input == desired))

-------------

bfl :: Tree -> Tree
bfl t = result
  where
    bfl' (Bin l _ r) (x:xs:xxs) =
      let
        (lr, xxs')  = bfl' l xxs
        (rr, xxs'') = bfl' r xxs''
      in 
        (Bin lr x rr, xs:xxs'')
    bfl' (Leaf) [1] = (Leaf, [1])
    (result, xxs) = bfl' t (1:xxs)

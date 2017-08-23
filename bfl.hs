import Data.List
import qualified Data.HashMap.Strict as M

data T = Leaf Int | Node T T deriving(Show, Eq, Ord)

input :: T
input = Node (Leaf 99) (Node (Node (Leaf 99) (Leaf 99)) (Leaf 99))

desired :: T
desired = Node (Leaf 1) (Node (Node (Leaf 3) (Leaf 4)) (Leaf 2))

main :: IO()
main =    putStrLn ("Input: " ++ (show input))
       >> putStrLn ("Output: " ++ (show $ lableBreadthFirst input))
       >> putStrLn ("Desired: " ++ (show desired))
       >> putStrLn ("Is desired? " ++ show (lableBreadthFirst input == desired))

-------------

lengths :: [Int]
lengths = map length $ sort $ group $ traverseDF $ lableWithLevels input

initialMap :: M.HashMap Int Int
initialMap = M.fromList $ zip [1..] (1 : map (+1) (scanl1 (+) lengths))

lableWithLevels :: T -> T
lableWithLevels t = labelWithLevels' t 0

labelWithLevels' :: T -> Int -> T
labelWithLevels' (Leaf _)   level = Leaf level 
labelWithLevels' (Node l r) level = Node (labelWithLevels' l (level+1)) (labelWithLevels' r (level+1))

lableBreadthFirst :: T -> T
lableBreadthFirst t = extractFirst $ lableBreadthFirst' (t, 0, initialMap)

lableBreadthFirst' :: (T, Int, M.HashMap Int Int) -> (T, Int, M.HashMap Int Int)
lableBreadthFirst' (Leaf _, level, m)   = (Leaf index, level, newM)
  where
    index = M.lookupDefault 0 (level) m
    newM = M.adjust (+1) (level) m
lableBreadthFirst' (Node l r, level, m) = (Node labelledLeft labelledRight, level, rightM)
  where
    (labelledRight, _, rightM) = lableBreadthFirst' (r, level + 1, leftM)
    (labelledLeft, _, leftM) = lableBreadthFirst' (l, level + 1, m)

traverseDF :: T -> [T]
traverseDF (Leaf i) = [Leaf i]
traverseDF (Node l r) = (traverseDF l) ++ (traverseDF r)

extractFirst :: (a, b, c) -> a
extractFirst (a,_,_) = a


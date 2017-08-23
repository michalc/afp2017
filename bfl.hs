import Data.List
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

data T = Leaf Int | Node T T deriving(Show, Eq, Ord)

input :: T
input = Node (Leaf 99) (Node (Node (Leaf 99) (Leaf 99)) (Leaf 99))

desired :: T
desired = Node (Leaf 1) (Node (Node (Leaf 3) (Leaf 4)) (Leaf 2))

main :: IO()
main =    putStrLn ("Input: " ++ (show input))
       >> putStrLn ("Output: " ++ (show $ lableBreadthFirst input))
       >> putStrLn ("Desired: " ++ (show desired))
       >> putStrLn ("initialMap: " ++ (show initialMap))
       >> putStrLn ("Is desired? " ++ show (lableBreadthFirst input == desired))

-------------

lengths :: [Int]
lengths = map length $ sort $ group $ traverseDF $ lableWithLevels input

initialMap :: V.Vector Int
initialMap = V.fromList $ 1 : map (+1) (scanl1 (+) lengths)

lableWithLevels :: T -> T
lableWithLevels t = labelWithLevels' t 0

labelWithLevels' :: T -> Int -> T
labelWithLevels' (Leaf _)   level = Leaf level 
labelWithLevels' (Node l r) level = Node (labelWithLevels' l (level+1)) (labelWithLevels' r (level+1))

lableBreadthFirst :: T -> T
lableBreadthFirst t = extractFirst $ lableBreadthFirst' (t, 0, initialMap)

lableBreadthFirst' :: (T, Int, V.Vector Int) -> (T, Int, V.Vector Int)
lableBreadthFirst' (Leaf _, level, v)   = (Leaf index, level, newV)
  where
    index = v V.! (level-1)
    -- This _might_ be creating a copy of the array, or it might not?
    newV = V.modify (\v -> MV.write v (level-1) (index + 1)) v
lableBreadthFirst' (Node l r, level, v) = (Node labelledLeft labelledRight, level, rightV)
  where
    (labelledRight, _, rightV) = lableBreadthFirst' (r, level + 1, leftV)
    (labelledLeft,  _, leftV)  = lableBreadthFirst' (l, level + 1, v)

traverseDF :: T -> [T]
traverseDF (Leaf i) = [Leaf i]
traverseDF (Node l r) = (traverseDF l) ++ (traverseDF r)

extractFirst :: (a, b, c) -> a
extractFirst (a,_,_) = a


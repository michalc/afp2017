{-# LANGUAGE DeriveDataTypeable #-}
-- Reimplement show and read using syb

import Data.Generics
import Data.List

data Tree = Leaf | Node Tree Int Tree deriving(Data, Typeable)

main :: IO()
main = putStrLn $ myShow (Node Leaf 5 (Node Leaf 2 Leaf))

myShow :: (Typeable b, Data b) => b -> String
myShow x = (showConstr $ toConstr x) ++
  if hasTerms then
    " (" ++ (intercalate " " $ gmapQ (\d -> myShow d) x) ++ ")"
  else
    ""
  where
    hasTerms = hasSubterms x

hasSubterms :: (Typeable b, Data b) => b -> Bool
hasSubterms = not . null . gmapQ (\x -> 1)

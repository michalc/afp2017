-- Exercise 3.1.1. Write a function parseTable :: [String] -> Table that parses a
-- table represented in its concrete syntax as a list of strings (each corresponding
-- to a single line in the input) into its abstract syntax. (Hint: use the function
-- words from the Prelude.)


type Field = String
type Row = [Field]
type Table = [Row]

main :: IO()
main = putStrLn $ show $ parseTable inputTable

parseTable :: [String] -> Table
parseTable = map words

inputTable = [
  "Alice Allen female 82000",
  "Bob Baker male 70000",
  "Carol Clarke female 50000",
  "Dan Davies male 45000",
  "Eve Evans female 275000"
  ]

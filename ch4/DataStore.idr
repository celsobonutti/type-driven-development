module DataStore

import Data.Vect
import Data.List
import Data.String
import System.REPL

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData size' items) = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) item = MkData _ $ snoc items item
  where
    snoc : Vect old type -> type -> Vect (S old) type
    snoc [] x = [x]
    snoc (y :: ys) x = y :: snoc ys x

data Command
  = Add String
  | Get Integer
  | Search String
  | Size
  | Quit

isNumber : String -> Bool
isNumber = all isDigit . unpack

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just . Add $ str
parseCommand "quit" "" = Just Quit
parseCommand "size" "" = Just Size
parseCommand "search" str = Just . Search $ str
parseCommand "get" index = if isNumber index
                              then Just . Get . cast $ index
                              else Nothing
parseCommand cmd args = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                (cmd, args) => parseCommand cmd (ltrim args)

getEntry : DataStore -> Integer -> Maybe (String, DataStore)
getEntry store pos = let storeItems = items store in
                           case integerToFin pos (size store) of
                             Nothing => Just ("Out of range\n", store)
                             (Just id) => Just (index id storeItems ++ "\n", store)

withIndexes : {n : _} -> Vect n String -> Vect n (Nat, String)
withIndexes xs = withIndexes' 0 xs
  where
    withIndexes' : {n : _} -> Nat -> Vect n type -> Vect n (Nat, type)
    withIndexes' k [] = []
    withIndexes' k (x :: xs) = (k, x) :: withIndexes' (S k) xs

listMatches : (store : DataStore) -> String -> List (Nat, String)
listMatches (MkData size' items) search = filter (isInfixOf search . snd) . toList . withIndexes $ items

searchStrs : DataStore -> String -> Maybe (String, DataStore)
searchStrs store search = Just (unlines . map (\(index, text) => show index ++ ": " ++ text) . listMatches store $ search, store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse input of
                            Nothing => Just ("Invalid command\n", store)
                            Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                            Just (Get index) => getEntry store index
                            Just Size => Just ("Size: " ++ show (size store) ++ "\n", store)
                            Just (Search str) => searchStrs store str
                            Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput

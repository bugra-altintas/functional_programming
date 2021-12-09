module PE4 where

import Data.Maybe () -- up to you if you want to use it or not

-- Generic DictTree definition with two type arguments
data DictTree k v = Node [(k, DictTree k v)] | Leaf v deriving Show

-- Lightweight Char wrapper as a 'safe' Digit type
newtype Digit = Digit Char deriving (Show, Eq, Ord) -- derive equality and comparison too!

-- Type aliases
type DigitTree = DictTree Digit String
type PhoneNumber = [Digit]


-- toDigit: Safely convert a character to a digit
toDigit :: Char -> Maybe Digit
toDigit chr | elem chr ['0','1','2','3','4','5','6','7','8','9'] = Just (Digit chr)
            | otherwise = Nothing

-- toDigits: Safely convert a bunch of characters to a list of digits.
--           Particularly, an empty string should fail.
toDigits :: String -> Maybe PhoneNumber
toDigits "" = Nothing
toDigits str | validStr str == False = Nothing
             | otherwise = Just [ extract (toDigit s) | s<-str]

extract Nothing = undefined --careful about it !!! do not send Nothing in it
extract (Just a) = a

validStr "" = True
validStr (s:str) = if (check (toDigit s)) then True && (validStr str) else False

check Nothing = False
check (Just a) = True

-- numContacts: Count the number of contacts in the phonebook...
numContacts :: DigitTree -> Int
numContacts (Leaf v) = 1
numContacts (Node []) = 0
numContacts (Node (child:childs)) = numContacts (snd child) + numContacts (Node childs)


isLeaf (key,Leaf value) = True
isLeaf _ = False

getValue (k,Leaf v) = v

    
-- getContacts: Generate the contacts and their phone numbers in order given a tree. 
getContacts :: DigitTree -> [(PhoneNumber, String)]
getContacts tree = path [] tree 

path _ (Leaf x) = [([],x)]
path _ (Node []) = []
path parent (Node (child:childs)) | isLeaf child = [(parent ++ [fst child],getValue child)] ++ path parent (Node childs)
                                  | otherwise = path (parent ++ [fst child]) (snd child) ++ path parent (Node childs)


-- autocomplete: Create an autocomplete list of contacts given a prefix
-- e.g. autocomplete "32" areaCodes -> 
--      [([Digit '2'], "Adana"), ([Digit '6'], "Hatay"), ([Digit '8'], "Osmaniye")]
autocomplete :: String -> DigitTree -> [(PhoneNumber, String)]
autocomplete "" _ = []
autocomplete route tree | validStr route == False = []
                        | otherwise = path2 [] (takeToTheNode (extract (toDigits route)) tree)

path2 _ (Node []) = []
path2 _ (Leaf x) | x == "can not go no more" = []
                 | otherwise = [([],x)]
path2 parent (Node (child:childs)) | isLeaf child = [(parent ++ [fst child],getValue child)] ++ path2 parent (Node childs)
                                   | otherwise = path2 (parent ++ [fst child]) (snd child) ++ path2 parent (Node childs)


takeToTheNode [] (Node t) = Node t
takeToTheNode _ (Node []) = (Leaf "can not go no more")
takeToTheNode route (Leaf x) | route == [] = (Leaf x)
                             | otherwise = (Leaf "can not go no more")
takeToTheNode (r:route) (Node (child:childs)) | (fst child) == r = takeToTheNode route (snd child)
                                              | otherwise = takeToTheNode (r:route) (Node childs)



-----------
-- Example Trees
-- Two example trees to play around with

exampleTree :: DigitTree
exampleTree = Node [
    (Digit '1', Node [
        (Digit '3', Node [
            (Digit '7', Node [
                (Digit '8', Leaf "Jones")])]),
        (Digit '5', Leaf "Steele"),
        (Digit '9', Node [
            (Digit '1', Leaf "Marlow"),
            (Digit '2', Node [
                (Digit '3', Leaf "Stewart")])])]),
    (Digit '3', Leaf "Church"),
    (Digit '7', Node [
        (Digit '2', Leaf "Curry"),
        (Digit '7', Leaf "Hughes")])]

areaCodes :: DigitTree
areaCodes = Node [
    (Digit '3', Node [
        (Digit '1', Node [
            (Digit '2', Leaf "Ankara")]),
        (Digit '2', Node [
            (Digit '2', Leaf "Adana"),
            (Digit '6', Leaf "Hatay"),
            (Digit '8', Leaf "Osmaniye")])]),
    (Digit '4', Node [
        (Digit '6', Node [
            (Digit '6', Leaf "Artvin")])])]
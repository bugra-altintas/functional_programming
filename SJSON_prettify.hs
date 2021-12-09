-- splitOn: Split string on first occurence of character.
splitOn :: Char -> String -> (String, String)
splitOn _ "" = ("","")
splitOn p str = (take (whereIs p str 0) str,drop ((whereIs p str 0) + 1) str)

whereIs :: Num t => Char -> [Char] -> t -> t
whereIs _ "" _ = 0 --finding the place of first occurence
whereIs p (s:str) n = if s == p then n else (whereIs p str n+1) 


-- tokenizeS: Transform an SJSON string into a list of tokens.
tokenizeS :: String -> [String]
tokenizeS str = splitS (splitOn '\'' str) 1

clearSpaces :: Integral a => [Char] -> a -> [Char]
clearSpaces str n = if (n `mod` 2 == 0) then str --to clear spaces
                        else [s | s<-str, s /= ' ',s/='\n', s/= '\t']
                        
charToString :: a -> [a]
charToString c = [c]

--splitting non-string parts one by one
parseNonS :: Integral a => String -> a -> [String]
parseNonS "" _ = [""] 
parseNonS str n = if (n `mod` 2 == 0) then [str]
                    else [charToString s| s<-str]

--splitting whole string according to the rules
splitS :: Integral t => (String, String) -> t -> [String]
splitS (str,"") _ = parseNonS (clearSpaces str 1) 1 
splitS (str1,str2) n =  (parseNonS (clearSpaces str1 n) n) ++ (splitS (splitOn '\'' str2) (n+1))

-- prettifyS: Prettify SJSON, better tokenize first!
prettifyS :: String -> String
prettifyS str = process (tokenizeS str) 0 0

--adjust the shape of string
process :: Integral a => [String] -> Int -> a -> [Char]
process [] _ _ = "" 
process (x:xs) indent nth | x == "{" = if (nth `mod` 2 == 0) then (sometabs indent) ++ (x ++ "\n") ++ (process xs (indent+1) 0)
                                        else (x ++ "\n") ++ (process xs (indent+1) 0)
                          | x == ":" = (x ++ " ") ++ (process xs indent nth)
                          | x == "," = (x ++ "\n") ++ (process xs indent nth)
                          | x == "}" = "\n" ++ (sometabs (indent-1)) ++ x ++ (process xs (indent-1) nth)
                          | otherwise = if (nth `mod` 2 == 0) then (sometabs indent) ++ ("\'" ++ x ++ "\'") ++ (process xs indent (nth+1))
                                            else ("\'" ++ x ++ "\'") ++ (process xs indent (nth+1))

--unify elements of a list, make it string
join :: [[Char]] -> [Char]
join [] = "" 
join (x:xs) = x ++ (join xs)

--tab function
sometabs :: Int -> [Char]
sometabs n = join (take n (repeat "    ")) 
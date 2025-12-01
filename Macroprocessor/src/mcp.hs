module MP (separators, lookUp, splitText, combine, getKeywordDefs, expand) where
import Data.Tuple (swap)

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators = " \n\t.,:;!\"\'()<>/\\"

-----------------------------------------------------

{-|
This function will look up a key in a list of key-value pairs,
returning all the values that match with that key.

> lookUp "A" [("A", 8), ("B", 9), ("C", 5), ("A", 7)] == [8, 7] 
-}
-- lookUp :: String -> [(String, a)] -> [a]
-- lookUp _ [] = []
-- lookUp p ((x, y):xys) 
--        |x == p = y:lookUp p xys
--        |otherwise = lookUp p xys

lookUp :: String -> [(String, a)] -> [a]
lookUp _ [] = []
lookUp p xys = [ y | (x, y) <- xys, x == p]


{-|
This function will break up a string with some given separator
characters, returning both the list of separators found between
each "word" and the words themselves.
-}

splitText :: [Char] -> String -> ([Char], [String])
splitText seps str = foldr step ([], [""]) str
       where
              step c (sepacc, wordacc)
                     | c `elem` seps = (c : sepacc, "" : wordacc)
                     | otherwise     = (sepacc, (c : head wordacc) : tail wordacc)


{-|
This function interleaves the characters from the first argument
list with the strings in the second argument. The second list must
be non-empty.
-}


combine :: [Char] -> [String] -> [String]
combine [] [w] = [w]
combine (s:ss) (w:ws) = [w] ++ [[s]] ++ combine ss ws
combine _ _ = [""]

{-|
This function takes a list of lines and splits each line to
extract a list of keyword-definition pairs.

> getKeywordDefs ["$x Define x", "$y 55"] == [("$x", "Define x"), ("$y", "55")]
-}
getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs [] = []
getKeywordDefs (l:lines)
       | null l = getKeywordDefs lines 
       | otherwise  = (kw, unwords def) : getKeywordDefs lines
              where
                     (_, kwdef) = splitText [' '] l
                     kw = head kwdef
                     def = tail kwdef



replaceWords :: KeywordDefs -> [String] -> String
replaceWords kwd = unwords . map swap
  where
    swap s@('$' : _) = maybe s id (lookup s kwd)
    swap s = s


{-|
This function takes the contents of two files, one containing
a template and the other the definitions for each keyword
found within the template. It extracts the keyword-definition
information from the info file and uses it to expand the occurrences
of these keywords in the template file, producing new file contents
as a result.

> expand "The capital $1 is $2" "$1 Peru\n$2 Lima." == "The capital of Peru is Lima"
-}
expand :: FileContents -- ^ the template file contents
       -> FileContents -- ^ the info file contents
       -> FileContents
expand temp info = concat (combine sepsused (words reps))
       where
              defs = getKeywordDefs (lines info)
              (sepsused , tokens) = splitText separators temp
              reps = replaceWords defs tokens

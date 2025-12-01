module CodeBreaker (secretMsg, guessKey) where

import Crypto (ecbDecrypt)
import Data.List

-------------------------------------------------------------------------------
-- PART 3 (Extension) : cracking the code

-- Shhh, it's a secret (this is a multi-line string, by the way)
secretMsg :: String
secretMsg = "cqnmnyjacvnwcxolxvydcrwprbcqnlxvydcnablrnwlnmnyjacvnwcjcrvynarjul\
            \xuunpnuxwmxwcqnmnyjacvnwcqjbjaxdwmorochjljmnvrlbcjoojwmxwncqxdbjw\
            \mbcdmnwcbfrcqjaxdwmbrgqdwmanmbcdmhrwpdwmnapajmdjcnlxdabnbcfxqdwma\
            \nmyqmbcdmnwcbjwmcfxqdwmanmvblbcdmnwcbcqnmnyjacvnwcrbyanmxvrwjwcuh\
            \kjbnmrwcqnqdgunhkdrumrwpfqrlqrcbqjanbfrcqcqnvjcqbmnyjacvnwcqxfnen\
            \ajubxqjbbyjlnrwcqnfruurjvynwwnhujkxajcxahjwmrwcqnjnaxwjdcrlbjwmlq\
            \nvrljunwprwnnarwpngcnwbrxwcqnmnyjacvnwcajwtbqrpquhrwdwrenabrchajw\
            \trwpcjkunb"

-- (Some of) the most common letter sequences of various sizes in English
enFreqSingle, enFreqBi, enFreqTri :: [String]
enFreqSingle = ["e", "t", "a", "o", "i", "n", "s", "h", "r"]
enFreqBi     = ["th", "he", "in", "er", "an", "re", "on", "at", "en"]
enFreqTri    = ["the", "and", "ing", "ent", "ion", "her", "for"]

ngrams :: Int -> [a] -> [[a]]
ngrams = undefined

freqs :: Int -> String -> [(String, Int)]
freqs = undefined

commonPrefixLength :: [String] -> [String] -> Int
commonPrefixLength = undefined

guessKey :: String -> [(Char, Int)]
guessKey = undefined

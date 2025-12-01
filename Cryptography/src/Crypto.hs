module Crypto ( gcd, smallestCoPrimeOf, computeCoeffs, inverse
              , modPow, genKeys, rsaEncrypt, rsaDecrypt
              , alphaOrd, alphaChr, (<+>), (<->)
              , ecbEncrypt, ecbDecrypt, cbcEncrypt, cbcDecrypt ) where

import Data.Char

import Prelude hiding (gcd)
import Data.Bits (Bits(xor))

{-
The advantage of symmetric encryption schemes like AES is that they are efficient
and we can encrypt data of arbitrary size. The problem is how to share the key.
The flaw of the RSA is that it is slow and we can only encrypt data of size lower
than the RSA modulus n, usually around 1024 bits (64 bits for this exercise!).

We usually encrypt messages with a private encryption scheme like AES-256 with
a symmetric key k. The key k of fixed size 256 bits for example is then exchanged
via the aymmetric RSA.
-}

-------------------------------------------------------------------------------
-- PART 1 : asymmetric encryption

-- | Returns the greatest common divisor of its two arguments
gcd :: Int -> Int -> Int
gcd m n
    | n == 0 = m
    |otherwise = gcd n (mod m n)

-- | Returns the smallest integer that is coprime with phi
smallestCoPrimeOf :: Int -> Int
smallestCoPrimeOf m = head [x | x <- [2..], gcd x m == 1]

-- | Calculates (n^k mod m)
modPow :: Int -> Int -> Int -> Int
modPow n k m
    |k == 0 = 1 `mod` m
    |even k = (modPow x j m) `mod` m
    |otherwise = (n*(modPow x j m)) `mod` m
    where
        j = (k `div` 2)
        x = (n^2 `mod` m)
    

-- | Computes Bézout coefficients for two integers
computeCoeffs :: Int -> Int -> (Int, Int)
computeCoeffs a b
    | b == 0 = (1, 0)
    | otherwise =
        let (q, r) = computeCoeffs b (a `mod` b)
        in (r, q - (a `div` b) * r)
        
-- | Inverse of n modulo m
inverse :: Int -> Int -> Int
inverse n m
    |gcd n m == 1 = fst(computeCoeffs n m) `mod` m

{-|
Generates keys pairs (public, private) = ((e, n), (d, n))
given two "large" distinct primes, p and q
-}
genKeys :: Int -> Int -> ((Int, Int), (Int, Int))
genKeys p q =
    ((e, n), (d, n))
    where
        e = smallestCoPrimeOf ((p-1)*(q-1))
        d = inverse e ((p-1)*(q-1))
        n = p * q

-- | This function performs RSA encryption
rsaEncrypt :: Int        -- ^ value to encrypt
           -> (Int, Int) -- ^ public key
           -> Int
rsaEncrypt x (e, n) = modPow x e n

-- | This function performs RSA decryption
rsaDecrypt :: Int        -- ^ value to decrypt
           -> (Int, Int) -- ^ private key
           -> Int
rsaDecrypt c (d, n) = modPow c d n

-------------------------------------------------------------------------------
-- PART 2 : symmetric encryption

-- | Returns position of a letter in the alphabet
alphaOrd :: Char -> Int
alphaOrd x 
    |ord x >= 65 && ord x <= 90 = ord x - base_upper
    |otherwise = ord x - base_lower
    where
        base_lower = ord 'a'
        base_upper = ord 'A'

-- | Returns the n^th letter
alphaChr :: Int -> Char
alphaChr c = 
    chr(c + jump_to_ascii)
    where
        jump_to_ascii = 97

-- | "adds" two letters
(<+>) :: Char -> Char -> Char
(<+>) b c 
    = alphaChr((d + e) `mod` max)
    where
        d = alphaOrd b
        e = alphaOrd c
        max = 26

-- | "subtracts" two letters
(<->) :: Char -> Char -> Char
(<->) b c 
    = alphaChr((d - e) `mod` max)
    where
        d = alphaOrd b
        e = alphaOrd c
        max = 26

-- the next functions present
-- 2 modes of operation for block ciphers : ECB and CBC
-- based on a symmetric encryption function e/d such as "add"

-- | ecb (electronic codebook) encryption with block size of a letter
ecbEncrypt :: Char -> [Char] -> [Char]
ecbEncrypt k m = [(<+>) mi k | mi <- m]



-- | ecb (electronic codebook) decryption with a block size of a letter
ecbDecrypt :: Char -> [Char] -> [Char]
ecbDecrypt k c = [(<->) ci k | ci <- c]

-- | cbc (cipherblock chaining) encryption with block size of a letter
cbcEncrypt :: Char   -- ^ public key
           -> Char   -- ^ initialisation vector `iv`
           -> [Char] -- ^ message `m`
           -> [Char]
cbcEncrypt _ _ [] = []
cbcEncrypt k iv (x:xs) = ci : cbcEncrypt k ci xs
    where
        ci = (<+>) k ((<+>) x iv)


-- | cbc (cipherblock chaining) decryption with block size of a letter
cbcDecrypt :: Char   -- ^ private key
           -> Char   -- ^ initialisation vector `iv`
           -> [Char] -- ^ message `m`
           -> [Char]
cbcDecrypt _ _ [] = []
cbcDecrypt k iv (c:cs) = xi : cbcDecrypt k c cs
    where
        xi = (<->) ((<->) c k) iv

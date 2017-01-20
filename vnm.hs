-- Middle square method for psuedo-random number generation.
-- https://en.wikipedia.org/wiki/Middle-square_method

import System.Environment
import System.Random
import Data.Bits
import Data.Word

vnmRandom :: Word8 -> Word16
vnmRandom x = fromIntegral $ shiftR (xSquared .&. (bitMask quarterBitSize halfBitSize)) quarterBitSize
  where xExt = fromIntegral x :: Word16
        halfBitSize = finiteBitSize xExt `div` 2
        quarterBitSize = halfBitSize `div` 2
        xSquared = xExt ^ 2

bitMask :: Int -> Int -> Word16
bitMask i n = (maskAllLowerBits i n) .&. (complement $ maskAllLowerBits i 0)

maskAllLowerBits :: Int -> Int -> Word16
maskAllLowerBits i n = (shiftL 1 (i + n)) - 1

decToBin :: Word16 -> String
decToBin 0 = ""
decToBin x = let (q, r) = quotRem x 2 in decToBin q ++ (show r)

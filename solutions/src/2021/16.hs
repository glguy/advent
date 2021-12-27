{-# Language ImportQualifiedPost, LambdaCase, QuasiQuotes, ViewPatterns, BlockArguments #-}
{-|
Module      : Main
Description : Day 16 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/16>

Decode an expression from a bitstream.

This solution uses the ReadP parser combinator module.
Since ReadP only operates on 'String' parsing bits is
simulated by parsing strings of the characters @'0'@ and @'1'@.
ReadP's 'ReadP.gather' operation is useful for computing
the bitsize of a packet which comes up in some of the
operator packets.

= Examples

>>> vers <$> parse (decodeHex "8A004A801A8002F478")
Just 16

>>> vers <$> parse (decodeHex "620080001611562C8802118E34")
Just 12

>>> vers <$> parse (decodeHex "C0015000016115A2E0802F182340")
Just 23

>>> vers <$> parse (decodeHex "A0016C880162017C3686B18A3D4780")
Just 31

>>> eval <$> parse (decodeHex "C200B40A82")
Just 3

>>> eval <$> parse (decodeHex "04005AC33890")
Just 54

>>> eval <$> parse (decodeHex "880086C3E88112")
Just 7

>>> eval <$> parse (decodeHex "CE00C43D881120")
Just 9

>>> eval <$> parse (decodeHex "D8005AC2A8F0")
Just 1

>>> eval <$> parse (decodeHex "F600BC2D8F")
Just 0

>>> eval <$> parse (decodeHex "9C005AC2F8F0")
Just 0

>>> eval <$> parse (decodeHex "9C0141080250320F1802104A08")
Just 1

-}
module Main (main) where

import Advent (fromDigits, format)
import Data.Char (digitToInt)
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as ReadP

-- | >>> :main
-- 843
-- 5390807940351
main :: IO ()
main =
 do inp <- [format|16 %s%n|]
    let Just p = parse (decodeHex inp)
    print (vers p)
    print (eval p)

-- | A /BITS/ packet
data Packet
  = Lit Int Int -- ^ Literal with version and value
  | Op Int Int [Packet] -- ^ Operator with version, opcode, and arguments
  deriving Show

-- | Compute the sum of the versions of all nested packets
vers :: Packet -> Int
vers (Lit v _   ) = v
vers (Op  v _ xs) = v + sum (map vers xs)

-- | Evaluate the packet as an expression
eval :: Packet -> Int
eval (Lit _ n     ) = n
eval (Op _ 0 xs   ) = sum      (eval <$> xs     )
eval (Op _ 1 xs   ) = product  (eval <$> xs     )
eval (Op _ 2 xs   ) = minimum  (eval <$> xs     )
eval (Op _ 3 xs   ) = maximum  (eval <$> xs     )
eval (Op _ 5 [x,y]) = fromEnum (eval x >  eval y)
eval (Op _ 6 [x,y]) = fromEnum (eval x <  eval y)
eval (Op _ 7 [x,y]) = fromEnum (eval x == eval y)
eval o = error ("bad expression: " ++ show o)

-- | Parser for strings of @'1'@ and @'0'@ to 'Packet'
--
-- >>> parse "110100101111111000101000"
-- Just (Lit 6 2021)
--
-- >>> parse "00111000000000000110111101000101001010010001001000000000"
-- Just (Op 1 6 [Lit 6 10,Lit 2 20])
--
-- >>> parse "11101110000000001101010000001100100000100011000001100000"
-- Just (Op 7 3 [Lit 2 1,Lit 4 2,Lit 1 3])
parse :: String -> Maybe Packet
parse (ReadP.readP_to_S pPacket -> [(p,_)]) = Just p
parse _ = Nothing

-- | Decode a hex string into bit string
--
-- >>> decodeHex "D2FE28"
-- "110100101111111000101000"
--
-- >>> decodeHex "38006F45291200"
-- "00111000000000000110111101000101001010010001001000000000"
decodeHex :: String -> String
decodeHex = concatMap \case
  '0' -> "0000"; '1' -> "0001"; '2' -> "0010"; '3' -> "0011"
  '4' -> "0100"; '5' -> "0101"; '6' -> "0110"; '7' -> "0111"
  '8' -> "1000"; '9' -> "1001"; 'A' -> "1010"; 'B' -> "1011"
  'C' -> "1100"; 'D' -> "1101"; 'E' -> "1110"; 'F' -> "1111"
  x -> error ("decodeHex: bad argument " ++ show x)

-- * 'ReadP' parser combinators

-- | Parse a single packet
pPacket :: ReadP Packet
pPacket =
 do v <- field 3; t <- field 3
    if t == 4
      then Lit v   <$> pLiteral
      else Op  v t <$> pArguments

-- | Parse an @n@-bit fixed-width big-endian, binary number
field :: Int {- ^ bit width -} -> ReadP Int
field n = fromDigits 2 . map digitToInt <$> ReadP.count n ReadP.get

-- | Parse a single bit as a boolean flag
flag :: ReadP Bool
flag = ('1' ==) <$> ReadP.get

-- | Parse a variable-sized number in 4-bit chunks
pLiteral :: ReadP Int
pLiteral = go 0
  where
    go acc =
     do more <- flag; chunk <- field 4
        (if more then go else pure) (16 * acc + chunk)

-- | Parse a list of sub-packets either by packet count or bit-size
pArguments :: ReadP [Packet]
pArguments =
 do mode <- flag
    if mode
      then do n <- field 11; ReadP.count n pPacket
      else do n <- field 15; pSized n

-- | Parse a list of packets that fit exactly in @n@ bits
pSized :: Int {- ^ bit width -} -> ReadP [Packet]
pSized n =
  case compare n 0 of
    LT -> ReadP.pfail
    GT -> do (str, p) <- ReadP.gather pPacket
             (p:) <$> pSized (n - length str)
    EQ -> pure []

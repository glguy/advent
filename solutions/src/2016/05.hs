module Main where

import           Crypto.Hash.MD5
import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B
import           System.IO
import           Text.Printf

main :: IO ()
main =
  do hSetBuffering stdout NoBuffering
     putStrLn password1
     putStrLn password2

input :: B.ByteString
input = B.pack "ffykfhsq"

passwordLen :: Int
passwordLen = 8

password1 :: String
password1 = fst <$> take passwordLen digitStream

password2 :: String
password2 = replicate passwordLen '_'
         ++ go 0 IntMap.empty digitStream'
  where
    digitStream' =
        [ (key, val) | (pos,val) <- digitStream
        , let key = fromEnum pos - fromEnum '0'
        , 0 <= key, key < passwordLen
        ]

    go _ seen _ | length seen == passwordLen = ""
    go _ _ [] = error "password generation underflow!"
    go n seen ((key,val) : rest)
      | IntMap.member key seen = render n seen ++ go (n+1) seen rest
      | otherwise              = render n seen' ++ go (n+1) seen' rest
      where
        seen' = IntMap.insert key val seen

spinner :: String
spinner    = "◐◓◑◒"

spinnerLen :: Int
spinnerLen = length spinner

render :: Int -> IntMap Char -> String
render n seen =
  '\r' : (spinner !! (n`rem`spinnerLen)) : ' ' :
  [ IntMap.findWithDefault '_' key seen | key <- [0 .. passwordLen - 1 ] ]

hexRep :: BS.ByteString -> String
hexRep bs = printf "%02x" =<< BS.unpack bs

digitStream :: [(Char,Char)]
digitStream = go (0 :: Int)
  where
    go i =
      case splitAt 5 (hexRep (hash (input <> B.pack (show i)))) of
        ("00000",c1:c2:_) -> (c1,c2) : go (i+1)
        _ -> go (i+1)

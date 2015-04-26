module Data.Base58String ( Base58String
                         , b58String
                         , fromBinary
                         , toBinary
                         , fromBytes
                         , toBytes
                         , toText ) where

import           Control.Applicative    ((<$>), pure)

import Data.Char (ord, chr)
import Data.Bits ((.|.), shiftL, shiftR)
import Data.List (unfoldr)

import Data.Maybe (fromJust, isJust, listToMaybe, fromMaybe)

import           Data.Aeson
import           Data.Word              (Word8)
import Numeric (showIntAtBase, readInt)
import Data.String (fromString)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BS8
import qualified Data.ByteString.Lazy   as BSL

import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE

import qualified Data.Binary            as B (Binary, decode, encode)

-- | Represents a Base58 string. Guarantees that all characters it contains
--   are valid base58 characters.
data Base58String =
  Base58String BS.ByteString
  deriving ( Show, Eq, Ord )

instance FromJSON Base58String where
  parseJSON = withText "Base58tring" $ pure . b58String . TE.encodeUtf8

instance ToJSON Base58String where
  toJSON = String . toText

-- | Smart constructor which validates that all the text are actually
--   base-58 characters.
b58String :: BS.ByteString -> Base58String
b58String bs =
  if   BS.all isValidBase58 bs
  then Base58String bs
  else error ("Not a valid base58 string: " ++ show bs)

-- | Converts a 'B.Binary' to a 'Base58String' value
fromBinary :: B.Binary a  => a -> Base58String
fromBinary = b58String . b58Encode . BSL.toStrict . B.encode

-- | Converts a 'Base58String' to a 'B.Binary' value
toBinary :: B.Binary a => Base58String -> a
toBinary (Base58String bs) = B.decode . BSL.fromStrict . fromMaybe (error "not a valid base58 input") $ b58Decode bs

-- | Reads a 'BS.ByteString' as raw bytes and converts to base58 representation. We
--   cannot use the instance Binary of 'BS.ByteString' because it provides
--   a leading length, which is not what we want when dealing with raw bytes.
fromBytes :: BS.ByteString -> Base58String
fromBytes = b58String . b58Encode

-- | Access to the raw bytes in a 'BS.ByteString' format.
toBytes :: Base58String -> BS.ByteString
toBytes (Base58String bs) = fromMaybe (error "not a valid base58 input") $ b58Decode bs

-- | Access to a 'T.Text' representation of the 'Base58String'
toText :: Base58String -> T.Text
toText (Base58String bs) = TE.decodeUtf8 bs

-- | Our mapping table from binary to base58
b58Table :: BS.ByteString
b58Table = BS.pack
           $  [49..57]
           ++ [65..72]
           ++ [74..78]
           ++ [80..90]
           ++ [97..107]
           ++ [109..122]

isValidBase58 :: Word8 -> Bool
isValidBase58 c =
  BS.elem c b58Table

b58 :: Word8 -> Word8
b58 i = BS.index b58Table (fromIntegral i)

b58' :: Word8 -> Maybe Word8
b58' w = fromIntegral <$> BS.elemIndex w b58Table

b58EncodeInt :: Integer -> BS.ByteString
b58EncodeInt i =
    fromString $ showIntAtBase (58 :: Integer) f (fromIntegral i) ""
  where
    f = chr . fromIntegral . b58 . fromIntegral

b58DecodeInt :: BS.ByteString -> Maybe Integer
b58DecodeInt s = case go of
    Just (r,[]) -> Just r
    _           -> Nothing
  where
    c = b58' . fromIntegral . ord
    p = isJust . c
    f = fromIntegral . fromJust . c
    go = listToMaybe $ readInt 58 p f (BS8.unpack s)

b58Encode :: BS.ByteString -> BS.ByteString
b58Encode input = BS.append l r
  where
    (z,b) = BS.span (== 0) input
    l = BS.map b58 z -- preserve leading 0's
    r | BS.null b = BS.empty
      | otherwise = b58EncodeInt $ bsToInteger b

b58Decode :: BS.ByteString -> Maybe BS.ByteString
b58Decode input = r >>= return . (BS.append prefix)
  where
    (z,b)  = BS.span (== (b58 0)) input
    prefix = BS.map (fromJust . b58') z -- preserve leading 1's
    r | BS.null b = Just BS.empty
      | otherwise = integerToBS <$> b58DecodeInt b

-- | Decode a big endian Integer from a bytestring
bsToInteger :: BS.ByteString -> Integer
bsToInteger = (foldr f 0) . reverse . BS.unpack
  where
    f w n = (toInteger w) .|. shiftL n 8

-- | Encode an Integer to a bytestring as big endian
integerToBS :: Integer -> BS.ByteString
integerToBS 0 = BS.pack [0]
integerToBS i
    | i > 0     = BS.pack $ reverse $ unfoldr f i
    | otherwise = error "integerToBS not defined for negative values"
  where
    f 0 = Nothing
    f x = Just $ (fromInteger x :: Word8, x `shiftR` 8)

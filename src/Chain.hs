{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Chain
  ( Hash
  , Link
  , Chain
  , empty
  , addBlock
  , verify
  ) where

import           Control.Monad (foldM)
import           Crypto.Hash (Digest(..), SHA256)
import qualified Crypto.Hash as SHA
import           Data.Aeson (ToJSON(..), FromJSON(..), genericToEncoding, defaultOptions, encode)
import           Data.Aeson.Types (Parser)
import           Data.ByteArray (convert)
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.ByteString.Lazy (toStrict)
import           Data.List (intercalate)
import           Data.Maybe (isJust)
import           GHC.Generics

-- | we are going to use SHA256-Hashes here
type Hash = Digest SHA256


-- | a chain is just a list of @Link
-- represented here in reversed order to make
-- adding blocks easier
-- it's expected that the block always starts with the @genesis link
data Chain block =
  Chain [Link block]
  deriving (Generic, Eq)

instance (ToJSON block, Show block) => Show (Chain block) where
  show (Chain ls) = intercalate ", " $ map show $ reverse ls

instance ToJSON block => ToJSON (Chain block) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON block => FromJSON (Chain block)


-- | empty Chain just consiting of the @genesis link
empty :: ToJSON block => Chain block
empty = Chain [genesis]


-- | adds a block to the chain, making sure the hashes line up
addBlock :: ToJSON block => block -> Chain block -> Chain block
addBlock !block (Chain ls@(h:_)) = Chain (newH:ls)
  where newH = link (Just $ hash h) (Just block)


-- | verifies the chain
-- to be valid it every links @hash must be correct and the
-- @prefHash field of a link must equal the @hash of it's predecessor in the chain
-- this is what a *Block-Chain* is all about
verify :: ToJSON block => Chain block -> Bool
verify (Chain ls) = isJust $ foldM checkLink Nothing $ reverse ls
  where
    checkLink prevH cur
      | prevH == prevHash cur && hashData prevH (block cur) == hash cur = Just (Just $ hash cur)
      | otherwise = Nothing


-- | a Chain-Link
data Link block = Link
  { prevHash :: !(Maybe Hash)   -- | the assumed has of the links predecessor - this one is included in the @hash
  , block    :: !(Maybe block)  -- | the data of the block - always non-@Nothing for all but the @genesis link
  , hash     :: !Hash           -- | the hash of the link computed with @prevHash and @block
  } deriving (Generic, Eq)


instance (ToJSON block, Show block) => Show (Link block) where
  show l = "{ block=" ++ show (block l) ++ ", prev=" ++ show (prevHash l) ++ ", hash=" ++ show (hash l) ++ " }"

instance ToJSON block => ToJSON (Link block) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON block => FromJSON (Link block)

-- | encode the string-rep of the digest via it's ByteString representation
instance ToJSON (Digest a) where
  toJSON dig = toJSON (unpack $ convert dig)

-- | decode by trying ByteString
instance forall a . SHA.HashAlgorithm a => FromJSON (Digest a) where
  parseJSON v = do
    bs <- pack <$> parseJSON v
    case SHA.digestFromByteString bs of
      Just h  -> return h
      Nothing -> error "could not read digest"


hashData :: ToJSON block => Maybe Hash -> Maybe block -> Hash
hashData p b = SHA.hash $ pack (show p) <> toStrict (encode b)


genesis :: ToJSON block => Link block
genesis = link Nothing Nothing


link :: ToJSON block => Maybe Hash -> Maybe block -> Link block
link pH bl = Link pH bl (hashData pH bl)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Unison.Runtime.Index
  (Unison.Runtime.Index.lookup
  ,Unison.Runtime.Index.delete
  ,Unison.Runtime.Index.insert
  ,Unison.Runtime.Index.lookupGT
  ,load
  ) where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import Data.Bytes.Serial (Serial)
import Data.Maybe (catMaybes)
import Data.Vector (Vector)
import GHC.Generics
import Unison.Cryptography (Cryptography)
import qualified Data.ByteString as ByteString
import qualified Data.Bytes.Serial as S
import qualified Data.Vector as V
import qualified Unison.BlockStore as BS
import qualified Unison.Cryptography as C
import qualified Unison.Runtime.Block as B

data TypedSeries a = TypedSeries { series :: BS.Series, defaultValue :: a } deriving Generic

data Trie = Trie { value :: TypedSeries (Maybe (Key, Value))
                 , branches :: Vector (TypedSeries (Maybe Trie))
                 } deriving Generic
{-
data IndexState h t1 t2 t3 t4 t5 t6 = IndexState
  { blockStore :: BS.BlockStore h
  , crypto :: Cryptography t1 t2 t3 t4 t5 t6 ByteString
  , trie :: TypedSeries Trie
  }
-}
data IndexState h t1 t2 t3 t4 t5 t6 = IndexState
  (BS.BlockStore h)
  (Cryptography t1 t2 t3 t4 t5 t6 ByteString)
  (TypedSeries Trie)

instance Serial BS.Series
instance Serial a => Serial (TypedSeries a)
instance Serial Trie where
  serialize (Trie value branches) = S.serialize value *> S.serialize (V.toList branches)
  deserialize = Trie <$> S.deserialize <*> fmap V.fromList S.deserialize

type KeyHash = ByteString
type Key = ByteString
type Value = ByteString

toBlock :: Serial a => Cryptography t1 t2 t3 t4 t5 t6 ByteString -> TypedSeries a -> B.Block a
toBlock crypto ts =
  B.serial (defaultValue ts) . B.encrypted crypto . B.fromSeries $ series ts

deserialize :: Serial a => BS.BlockStore h -> Cryptography t1 t2 t3 t4 t5 t6 ByteString -> TypedSeries a -> IO a
deserialize bs crypto ts = B.get bs $ toBlock crypto ts

randomSeries :: Cryptography t1 t2 t3 t4 t5 t6 ByteString -> IO BS.Series
randomSeries crypto = BS.Series <$> C.randomBytes crypto 64

emptyTT :: Eq a => BS.BlockStore a -> Cryptography t1 t2 t3 t4 t5 t6 ByteString
  -> IO (TypedSeries Trie)
emptyTT bs crypto = do
  rootSeries@(BS.Series bytes) <- randomSeries crypto
  serieses <- replicateM 256 $ randomSeries crypto
  h0 <- BS.declareSeries bs rootSeries
  let
    valueSeries = TypedSeries (BS.Series $ bytes `mappend` "values") Nothing
    blocks :: [TypedSeries (Maybe Trie)]
    blocks = map (`TypedSeries` Nothing) serieses
    node = Trie valueSeries . V.fromList $ blocks
    typedSeries = TypedSeries rootSeries node
    block = toBlock crypto typedSeries
  _ <- B.tryUpdate bs block h0 node
  pure typedSeries

isEmpty :: BS.BlockStore a -> Cryptography t1 t2 t3 t4 t5 t6 ByteString -> Trie -> IO Bool
isEmpty bs crypto trie = do
  dBranches <- mapM (deserialize bs crypto) . V.toList $ branches trie
  pure . null. catMaybes $ dBranches

load :: Eq a => BS.BlockStore a -> Cryptography t1 t2 t3 t4 t5 t6 ByteString -> BS.Series
  -> IO (IndexState a t1 t2 t3 t4 t5 t6)
load bs crypto series = do
  let block = B.fromSeries series
      blockTyped = B.serialM (emptyTT bs crypto) block
  trie <- B.get bs blockTyped
  pure $ IndexState bs crypto trie

insert :: Eq a => IndexState a t1 t2 t3 t4 t5 t6 -> KeyHash -> (Key, Value) -> IO ()
insert (IndexState bs crypto trie) kh (k,v) = do
  trie <- deserialize bs crypto trie
  insert' trie kh
  where
    insert' trie kh = case ByteString.uncons kh of
      Nothing -> let block = toBlock crypto $ value trie
                in void (B.modify' bs block $ const (Just (k,v)))
      Just (branchKey, kh) -> do
        let typedSeries = branches trie V.! fromIntegral branchKey
        branch <- deserialize bs crypto typedSeries
        case branch of
          Nothing -> do
            newBranch <- deserialize bs crypto =<< emptyTT bs crypto
            _ <- B.modify' bs (toBlock crypto typedSeries) (const $ Just newBranch)
            insert' newBranch kh
          Just branch -> insert' branch kh

delete :: Eq a => IndexState a t1 t2 t3 t4 t5 t6 -> KeyHash -> IO ()
delete (IndexState bs crypto trie) k = do
  Trie v0 branches <- deserialize bs crypto trie
  case ByteString.uncons k of
    Nothing -> void (B.modify' bs (toBlock crypto v0) $ const Nothing)
    Just (branchKey, k) -> delete' (branches V.! fromIntegral branchKey) k
  where
    delete' trie k = do
      trieDeserialized <- deserialize bs crypto trie
      case (ByteString.uncons k, trieDeserialized) of
        (Nothing, Just (Trie v0 _)) -> void (B.modify' bs (toBlock crypto v0) $ const Nothing)
        (Just (branchKey, k), Just trie2@(Trie _ branches)) -> do
          delete' (branches V.! fromIntegral branchKey) k
          -- calculating if the tree is empty is expensive.
          -- TODO if it fails, earlier calls in the recursion stack should automatically fail
          emptyTree <- isEmpty bs crypto trie2
          when emptyTree $ BS.deleteSeries bs (series trie)
        _ -> pure () -- key didn't actually exist in this index

lookup :: Eq a => IndexState a t1 t2 t3 t4 t5 t6 -> KeyHash -> IO (Maybe (Key,Value))
lookup (IndexState bs crypto trie) k = do
  trie <- deserialize bs crypto trie
  lookup' trie k
  where
    lookup' (Trie v0 branches) k = case ByteString.uncons k of
      Nothing -> let block = toBlock crypto v0
                 in B.get bs block
      Just (branchKey, k) -> do
        let typedSeries = branches V.! fromIntegral branchKey
        branch <- deserialize bs crypto typedSeries
        case branch of
          Nothing -> pure Nothing
          Just branch -> lookup' branch k

lookupGTE :: Eq a => IndexState a t1 t2 t3 t4 t5 t6 -> KeyHash -> IO (Maybe (KeyHash, (Key, Value)))
lookupGTE (IndexState bs crypto trie) k = do
  trie <- deserialize bs crypto trie
  lookupGT' trie k ByteString.empty
  where
    lookupGT' (Trie v0 branches) k kcons = case ByteString.uncons k of
      Nothing -> let block = toBlock crypto v0 in do
                 --in (\kv -> Just (kcons, kv)) <$> B.get bs block
        result <- B.get bs block
        pure $ case result of
          Nothing -> Nothing
          Just result -> Just (kcons, result)
      Just (branchKey, k) -> do
        let makeOption i = do
              branch <- deserialize bs crypto $ branches V.! fromIntegral i
              -- TODO remove case statement for something more succinct
              case branch of
                Nothing -> pure Nothing
                Just branch -> lookupGT' branch k $ ByteString.snoc kcons i
        foldM (\m i -> liftM2 (<|>) (pure m) (makeOption i)) Nothing
          [branchKey..255]

succB :: ByteString -> ByteString
succB b = case ByteString.unsnoc b of
  Nothing -> ByteString.singleton 0
  Just (init, last) -> uncurry ByteString.snoc $
    if last == 255 then (succB init, 0) else (init, last + 1)

lookupGT :: Eq a => IndexState a t1 t2 t3 t4 t5 t6 -> KeyHash -> IO (Maybe (KeyHash, (Key, Value)))
lookupGT indexState k = lookupGTE indexState $ succB k


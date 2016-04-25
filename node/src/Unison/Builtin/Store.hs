{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Unison.Builtin.Store where

import Control.Monad.State
import Control.Monad.Reader
import Data.Acid
import Data.Acid.Advanced
import Unison.Node.Builtin
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Text as Text
import qualified Unison.Eval.Interpreter as I
import qualified Unison.Note as Note
import qualified Unison.Reference as R
import qualified Unison.Term as Term
import qualified Unison.Type as Type

import Data.SafeCopy

import Data.Typeable

import qualified Data.Map as Map

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

type Key = String
type Value = String

data KeyValue = KeyValue !(Map.Map Key Value)
    deriving (Typeable)

$(deriveSafeCopy 0 'base ''KeyValue)

------------------------------------------------------
-- The transaction we will execute over the state.

insertKey :: Key -> Value -> Update KeyValue ()
insertKey key value
    = do KeyValue m <- get
         put (KeyValue (Map.insert key value m))

lookupKey :: Key -> Query KeyValue (Maybe Value)
lookupKey key
    = do KeyValue m <- ask
         return (Map.lookup key m)

$(makeAcidic ''KeyValue ['insertKey, 'lookupKey])

data InternalState = InternalState { stringStore :: AcidState KeyValue
                                   }

--store k v = Type.app (Type.app (Type.ref (R.Builtin "Store")) k) v
store k v = Type.ref (R.Builtin "Store") `Type.app` k `Type.app` v

makeAPI :: IO (WHNFEval -> [Builtin])
makeAPI = do
  stringStore <- MVar.newMVar Nothing
  let getAndSetStore = do
      storeState <- MVar.readMVar stringStore
      case storeState of
        Nothing -> do
          ss <- openLocalState (KeyValue Map.empty)
          MVar.putMVar stringStore $ Just ss
          pure ss
        Just ss -> pure ss
  pure (\whnf -> map (\(r, o, t, m) -> Builtin r o t m)
     [ let r = R.Builtin "KeyValue.store"
           op [] = do
             -- TODO - we should have the stringStore initialized here, once we have multiple
             -- types of key/value stores
             pure . Term.lit $ Term.KeyValueStore
           op _ = fail "KeyValue.store unpossible"
       in (r, Just (I.Primop 0 op), remote (store str str), prefix "stringStore")
     , let r = R.Builtin "KeyValue.lookup"
           op [indexToken, key] = inject g indexToken key where
             inject g indexToken key = do
               i <- whnf indexToken
               k <- whnf key
               g i k
             -- since there's only one index, indexToken is ignored right now
             g Term.Store' (Term.Text' t) = do
               val <- Note.lift $ do
                 ss <- getAndSetStore
                 result <- query ss (LookupKey $ Text.unpack t)
                 pure $ maybe "" Text.pack result -- TODO introduce maybe type to unison
               pure . Term.lit $ Term.Text val
             g s k = pure $ Term.ref r `Term.app` s `Term.app` k
           op _ = fail "KeyValue.lookup unpossible"
       in (r, Just (I.Primop 2 op), str --> store str str --> remote str, prefix "lookupKey")
     , let r = R.Builtin "KeyValue.insert"
           op [k, v, store] = inject g k v store where
             inject g k v store = do
               k' <- whnf k
               v' <- whnf v
               s <- whnf store
               g k' v' s
             g (Term.Text' k) (Term.Text' v) Term.Store' = do
               Note.lift $ do
                ss <- getAndSetStore
                update ss (InsertKey (Text.unpack k) (Text.unpack v))
               pure unitRef
             g k v store = pure $ Term.ref r `Term.app` k `Term.app` v `Term.app` store
           op _ = fail "KeyValue.insert unpossible"
       in (r, Just (I.Primop 3 op), str --> str --> store str str --> remote unitT, prefix "insertKey")
     ])


{--


data Index k v

index : Remote! (Index k v)
lookup : k -> Index k v -> Remote! (Maybe v)
delete : k -> Index k v -> Remote! ()
-- keys : Index k v -> Stream Remote! k

--}

{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Simple visualisation of a mockchain
module Wallet.Visualisation(
    startVis,
    sendOwners,
    sendFlowDiagram,
    sendTimeSeries
    ) where

import           Data.Aeson             hiding (Value)
import           Data.Aeson.Types       (toJSONKeyText)
import           Data.Foldable          (fold, traverse_)
import qualified Data.Map               as Map
import           Data.Semigroup         (Semigroup (..))
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import           Lens.Micro.Extras
import           Wallet.UTXO

import           Control.Concurrent     (MVar, ThreadId, forkIO, modifyMVar_, newMVar, readMVar)
import           Control.Exception.Base (catch)
import           Network.WebSockets     (ConnectionOptions (..), defaultConnectionOptions)
import qualified Network.WebSockets     as WS

-- | Owner of unspent funds
data UtxOwner =
    PubKeyOwner PubKey
    -- ^ Funds owned by a known public key
    | ScriptOwner
    -- ^ Funds locked by script
    | OtherOwner
    -- ^ All other funds (that is, funds owned by a public key we are not interested in )
    deriving (Eq, Ord, Show)

utxOwnerText :: UtxOwner -> Text.Text
utxOwnerText = \case
    PubKeyOwner (PubKey i) -> Text.pack ("pubkey-" <> show i)
    ScriptOwner -> "script"
    OtherOwner -> "other"

keysToOwners :: [PubKey] -> [UtxOwner]
keysToOwners ks = ScriptOwner:OtherOwner:(PubKeyOwner <$> ks)

instance ToJSON UtxOwner where
    -- | Generates JSON that is easy to use with D3.js on the client
    toJSON = toJSON . utxOwnerText

instance ToJSONKey UtxOwner where
    toJSONKey = toJSONKeyText utxOwnerText


-- | The total value in a blockchain grouped by [[UtxOwner]]
utxoByPk :: [PubKey] -> Blockchain -> Map.Map UtxOwner [Integer]
utxoByPk keys bc = result where
    knownKeys = Set.fromList keys

    result = foldr (flip (Map.unionWith (++))) Map.empty (Map.map return . blockMap <$> bc)

    blockMap :: Block -> Map.Map UtxOwner Integer
    blockMap = foldr (Map.unionWith (+)) Map.empty
        . fmap extract
        . foldMap (view outputs)

    extract :: TxOut h -> Map.Map UtxOwner Integer
    extract t = Map.singleton (owner knownKeys t) (fromIntegral $ txOutValue t)

owner :: Set.Set PubKey -> TxOut h -> UtxOwner
owner keys TxOut{..} = case txOutType of
    PayToScript _ _ -> ScriptOwner
    PayToPubKey pk
        | pk `Set.member` keys -> PubKeyOwner pk
    _ -> OtherOwner

-- | Wrapper around the first 8 digits of a `TxId'`
newtype TxRef = TxRef Text.Text

instance ToJSON TxRef where
    toJSON (TxRef t) = toJSON t

mkRef :: TxId' -> TxRef
mkRef = TxRef . Text.pack . take 8 . show . getTxId

data FlowLink = FlowLink
    { flowLinkSource :: TxRef
    , flowLinkTarget :: TxRef
    , flowLinkValue  :: Integer
    , flowLinkOwner  :: UtxOwner
    }

instance ToJSON FlowLink where
    -- | Generates JSON that is easy to use with D3.js on the client
    toJSON FlowLink{..} = object [
        "source" .= toJSON flowLinkSource,
        "target" .= toJSON flowLinkTarget,
        "value"  .= toJSON flowLinkValue,
        "owner"  .= toJSON flowLinkOwner]

-- | The flows of value from t
txnFlows :: [PubKey] -> Blockchain -> [FlowLink]
txnFlows keys = foldMap extract . fold where
    knownKeys = Set.fromList keys

    extract tx = fmap flow (txOutRefs tx) where
        tgt = mkRef $ hashTx tx
        flow (txo, txor) = FlowLink
                            { flowLinkSource = mkRef $ txOutRefId txor
                            , flowLinkTarget = tgt
                            , flowLinkValue  = fromIntegral $ txOutValue txo
                            , flowLinkOwner  = owner knownKeys txo
                            }

data Message =
    KnownOwners [UtxOwner]
    -- ^ Inform the visualisation of the owners of funds we expect to deal with
    --   (this is to allow the initialisation of a colour scale)
    | FlowDiagram [FlowLink]
    -- ^ Show a diagram of the money flows
    | TimeSeries (Map.Map UtxOwner [Integer])
    -- ^ Show a diagram of the total funds available to each wallet

tag :: Message -> String
tag = \case
    KnownOwners _ -> "known_owners"
    FlowDiagram _ -> "flow_diagram"
    TimeSeries _ -> "time_series"

instance ToJSON Message where
    toJSON m = object ["tag" .= toJSON (tag m), "data" .= dt] where
        dt = case m of
                KnownOwners os -> toJSON os
                FlowDiagram fd -> toJSON fd
                TimeSeries  mp -> toJSON mp

data VisState = VisState
                { visStateClients :: Map.Map Int WS.Connection
                , visStateOwners  :: [PubKey]
                }

emptyVisState :: VisState
emptyVisState = VisState Map.empty []

-- | Add a client and return its ID
addClient :: WS.Connection -> VisState -> VisState
addClient c (VisState v o) = VisState (Map.insert k c v) o where
    k = maybe 0 (succ . fst) (Map.lookupMax v)

-- | Remove a client
removeClient :: Int -> VisState -> VisState
removeClient i (VisState m o) = VisState (Map.delete i m) o

-- | Start the visualisation server on 127.0.0.1:9161
startVis :: IO (ThreadId, MVar VisState)
startVis = do
    let opts = defaultConnectionOptions { 
          connectionOnPong = putStrLn "Pong"
        , connectionCompressionOptions  = WS.PermessageDeflateCompression WS.defaultPermessageDeflate 
        }
    st <- newMVar emptyVisState
    i <- forkIO $ WS.runServerWith  "127.0.0.1" 9161 opts $ \pending -> do
        let rq  = WS.pendingRequest pending
        conn <- WS.acceptRequest pending
        -- WS.forkPingThread conn 30
        putStrLn "Connection accepted"
        putStrLn $ "Requested subprotocols: " ++ show (WS.getRequestSubprotocols rq)
        putStrLn $ "Request headers:        " ++ show (WS.requestHeaders rq)
        modifyMVar_ st (pure . addClient conn)
        readMVar st >>= sendOwners st . visStateOwners
        _ :: Text.Text <- WS.receiveData conn
        return ()
    return (i, st)

sendMsg :: MVar VisState -> Message -> IO ()
sendMsg mv msg = readMVar mv >>= sendAll where
    disconnect i = modifyMVar_ mv (pure . removeClient i)

    sendAll (VisState mp _) = traverse_ send $ Map.toList mp
    send (i, conn) = (WS.sendTextData conn $ encode msg) `catch` handleClose i

    handleClose i (WS.CloseRequest i' msg') = do
        putStrLn $ "Received close request " ++ show i' ++ " : " ++ show msg'
        disconnect i
    handleClose i WS.ConnectionClosed = do
        putStrLn "Unexpected connection closed exception"
        disconnect i
    handleClose i (WS.ParseException e) = do
        putStrLn $ "Recevied parse exception: " ++ show e
        disconnect i
    handleClose i (WS.UnicodeException e) = do
        putStrLn $ "Recevied unicode exception: " ++ show e
        disconnect i

-- | Inform clients about the list of known owners of funds
sendOwners :: MVar VisState -> [PubKey] -> IO ()
sendOwners mv os = do
    modifyMVar_ mv $ \vs -> pure (vs { visStateOwners = os })
    sendMsg mv (KnownOwners $ keysToOwners os)

-- | Send the data needed for a flow diagram of transactions
sendFlowDiagram :: MVar VisState -> Blockchain -> IO ()
sendFlowDiagram mv bc = do
    VisState{..} <- readMVar mv
    sendMsg mv (FlowDiagram $ txnFlows visStateOwners bc)

-- | Send the data needed for a time series of funds
sendTimeSeries :: MVar VisState -> Blockchain -> IO ()
sendTimeSeries mv bc = do
    VisState{..} <- readMVar mv
    sendMsg mv (TimeSeries $ utxoByPk visStateOwners bc)

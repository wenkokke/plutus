{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
-- | Simple visualisation of a mockchain
module Wallet.Visualisation(
    startVis,
    sendOwners,
    sendFlowDiagram,
    sendTimeSeries,
    initialChain,
    commitFunds,
    collectFunds,
    -- * Running traces
    runTraceClient,
    updateAll,
    -- *
    module Wallet.UTXO
    ) where

import           Control.Monad          (void)
import           Data.Aeson             hiding (Value)
import           Data.Aeson.Types       (toJSONKeyText)
import           Data.Foldable          (fold, traverse_)
import           Data.List              (nub)
import qualified Data.Map               as Map
import           Data.Semigroup         (Semigroup (..))
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import           Lens.Micro.Extras
import qualified Wallet.Emulator.Types  as Emulator
import           Wallet.UTXO

import           Control.Concurrent     (MVar, forkIO, modifyMVar_, newMVar, readMVar)
import           Control.Exception.Base (catch)
import           Network.WebSockets     (defaultConnectionOptions)
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
    PubKeyOwner (PubKey i) -> Text.pack ("wallet-" <> show i)
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
    deriving (Eq, Ord, Show)

instance ToJSON TxRef where
    toJSON (TxRef t) = toJSON t

mkRef :: TxId' -> TxRef
mkRef = TxRef . Text.pack . take 8 . show . getTxId


data FlowLink = FlowLink
    { flowLinkSource :: TxRef
    , flowLinkTarget :: TxRef
    , flowLinkValue  :: Integer
    , flowLinkOwner  :: UtxOwner
    } deriving Show

-- TODO: Node with objects
instance ToJSON FlowLink where
    -- | Generates JSON that is easy to use with D3.js on the client
    toJSON FlowLink{..} = object [
        "source" .= toJSON flowLinkSource,
        "target" .= toJSON flowLinkTarget,
        "value"  .= toJSON flowLinkValue,
        "owner"  .= toJSON flowLinkOwner]

data FlowGraph = FlowGraph
    { flowGraphLinks :: [FlowLink]
    , flowGraphNodes :: [TxRef]
    }

instance ToJSON FlowGraph where
    toJSON FlowGraph{..} = object [
        "nodes" .= toJSON (mkNode <$> flowGraphNodes),
        "links" .= toJSON flowGraphLinks
        ] where
            mkNode t = object ["name" .= toJSON t]

graph :: [FlowLink] -> FlowGraph
graph lnks = FlowGraph{..} where
    flowGraphLinks = lnks
    flowGraphNodes = nub $ fmap flowLinkSource lnks ++ fmap flowLinkTarget lnks

-- | The flows of value from t
txnFlows :: [PubKey] -> Blockchain -> [FlowLink]
txnFlows keys bc = utxoLinks ++ (foldMap extract $ fold bc) where
    knownKeys = Set.fromList keys
    getOut rf = let Just o = out bc rf in o
    utxos = fmap fst $ Map.toList $ unspentOutputs bc
    utxoLinks = uncurry flow <$> zip (utxoTargets <$> utxos) utxos
    utxoTargets (TxOutRef rf idx) = 
        TxRef $ Text.unwords [
            "utxo", 
            Text.pack $ take 8 $ show $getTxId rf, 
            Text.pack $ show idx]

    extract tx = fmap (flow (mkRef $ hashTx tx) . txInRef) (Set.toList $ txInputs tx) 

    -- make a flow for a TxOutRef
    flow tgt rf = 
        let src = getOut rf in
            FlowLink
                { flowLinkSource = mkRef $ txOutRefId rf -- source :: TxRe
                , flowLinkTarget = tgt -- target :: TxRef
                , flowLinkValue  = fromIntegral $ txOutValue src
                , flowLinkOwner  = owner knownKeys src
                }

data Message =
    KnownOwners [UtxOwner]
    -- ^ Inform the visualisation of the owners of funds we expect to deal with
    --   (this is to allow the initialisation of a colour scale)
    | FlowDiagram FlowGraph
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
startVis :: IO (MVar VisState)
startVis = do
    let opts = defaultConnectionOptions
    st <- newMVar emptyVisState
    _ <- forkIO $ WS.runServerWith  "127.0.0.1" 9161 opts $ \pending -> do
        conn <- WS.acceptRequest pending
        modifyMVar_ st (pure . addClient conn)
        readMVar st >>= sendOwners st . visStateOwners

        -- Blocks forever; the connection is closed when this thread ends
        _ :: Text.Text <- WS.receiveData conn
        return ()
    return st

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
        putStrLn $ "Received parse exception: " ++ show e
        disconnect i
    handleClose i (WS.UnicodeException e) = do
        putStrLn $ "Received unicode exception: " ++ show e
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
    sendMsg mv (FlowDiagram $ graph $ txnFlows visStateOwners bc)

-- | Send the data needed for a time series of funds
sendTimeSeries :: MVar VisState -> Blockchain -> IO ()
sendTimeSeries mv bc = do
    VisState{..} <- readMVar mv
    sendMsg mv (TimeSeries $ utxoByPk visStateOwners bc)

mkLink :: Text.Text -> Text.Text -> Integer -> Int -> FlowLink
mkLink a b v o = FlowLink (TxRef a) (TxRef b) v (PubKeyOwner (PubKey o))

mkScriptLink :: Text.Text -> Text.Text -> Integer -> FlowLink
mkScriptLink a b v = FlowLink (TxRef a) (TxRef b) v ScriptOwner

flows1 :: [FlowLink]
flows1 = [
    mkLink "a" "b" 100 1,
    mkLink "a" "c" 100 2,
    mkLink "a" "d" 100 3,
    mkLink "a" "e" 100 4
    ]

initialChain :: MVar VisState -> IO ()
initialChain mv = do
    sendOwners mv (PubKey <$> [1, 2, 3, 4])
    sendMsg mv (FlowDiagram $ graph flows1)

flows2 :: [FlowLink]
flows2 = flows1 ++ [
    mkScriptLink "e" "f" 80,
    mkLink "e" "g" 20 4,
    mkScriptLink "d" "f" 70,
    mkLink "d" "h" 30 3]

commitFunds :: MVar VisState -> IO ()
commitFunds mv = do
    sendOwners mv (PubKey <$> [1, 2, 3, 4])
    sendMsg mv (FlowDiagram $ graph flows2)

flows3 :: [FlowLink]
flows3 = flows2 ++ [
    mkLink "f" "i" 150 2
    ]

collectFunds :: MVar VisState -> IO ()
collectFunds mv = do
    sendOwners mv (PubKey <$> [1, 2, 3, 4])
    sendMsg mv (FlowDiagram $ graph flows3)

-- | Run a trace and send the resulting mockchain down the socket
runTraceClient :: MVar VisState -> Emulator.Trace Emulator.EmulatedWalletApi () -> IO ()
runTraceClient mv tr = do
    let initialTX = Tx {
            txInputs = Set.empty,
            txOutputs = pubKeyTxOut initialBalance <$> keys,
            txForge = initialBalance * fromIntegral (length keys),
            txFee = 0,
            txSignatures = []
            }
        initialBalance = 50000
        keys = PubKey <$> [1, 2, 3, 4]
        (e, st) = Emulator.runTraceTxPool [initialTX] (updateAll >> tr)
        ch = Emulator.emChain st
    _ <- either print pure e
    sendOwners mv (PubKey <$> [1, 2, 3, 4])
    sendFlowDiagram mv ch

-- | Update all known wallets in the visualisation
updateAll :: Emulator.Trace Emulator.EmulatedWalletApi ()
updateAll = Emulator.blockchainActions >>= void . Emulator.walletsNotifyBlock wlts where
    wlts = Emulator.Wallet <$> [1..4]

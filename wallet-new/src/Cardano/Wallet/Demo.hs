{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Demo
    (
    -- * NodeName
    NodeName(..)

    -- * Start a (demo) cluster of wallet nodes
    , startDemoCluster
    , startDemoNode
    , waitForNode

    -- * Demo Configurations
    , demoTopology
    , demoTLSConfiguration
    , prepareDemoEnvironment
    ) where

import qualified Prelude
import           Universum hiding (keys, takeWhile, (%~), (.~))

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (Async, async)
import           Control.Lens (at, (%~), (.~), (?~))
import qualified Data.Aeson as Aeson
import           Data.Attoparsec.ByteString.Char8 (IResult (..), parse,
                     skipWhile, string, takeWhile)
import qualified Data.Attoparsec.Internal.Types as Atto.Internal
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import           Data.List (elemIndex, stripPrefix, (\\))
import           Data.Map (Map, (!))
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Options.Applicative (ParseError (ShowHelpText), Parser,
                     ParserHelp (..), ParserInfo, execFailure, execParserPure,
                     handleParseResult, info, parserFailure)
import qualified Options.Applicative as Parser
import           Options.Applicative.Help.Chunk (Chunk (..))
import           System.Directory (createDirectoryIfMissing)
import           System.Environment (getEnvironment)
import           System.FilePath.Posix (takeDirectory, (</>))
import           System.IO.Temp (withSystemTempDirectory)

import           Cardano.Wallet.Action (actionWithWallet)
import           Cardano.Wallet.API.V1.Types (ForceNtpCheck (..))
import           Cardano.Wallet.Client (ClientError (..), ServantError (..),
                     WalletClient (getNodeInfo))
import           Cardano.Wallet.Server.CLI (NewWalletBackendParams (..),
                     walletBackendParamsParser)
import           Cardano.Wallet.Util (getsModify, kToS, sToK,
                     unsafeBoolFromString, unsafeIPFromString,
                     unsafeNetworkAddressFromString, unsafeSeverityFromString,
                     (|>))
import           Cardano.X509.Configuration (CertConfiguration (..),
                     CertDescription (..), DirConfiguration (..),
                     ServerConfiguration (..), TLSConfiguration (..),
                     fromConfiguration, genCertificate)
import           Data.X509.Extra (genRSA256KeyPair, writeCertificate,
                     writeCredentials)
import           Network.Broadcast.OutboundQueue (MaxBucketSize (..))
import           Pos.Client.CLI.NodeOptions (commonNodeArgsParser,
                     nodeArgsParser)
import           Pos.Client.CLI.Params (loggingParams)
import           Pos.Core.NetworkAddress (NetworkAddress)
import           Pos.Infra.Network.DnsDomains (DnsDomains (..), NodeAddr (..))
import           Pos.Infra.Network.Types (NodeName (..), NodeType (..))
import           Pos.Infra.Network.Yaml (AllStaticallyKnownPeers (..),
                     NodeMetadata (..), NodeRegion (..), NodeRoutes (..),
                     Topology (..))
import           Pos.Launcher (LoggingParams (..), launchNode)
import           Pos.Util.CompileInfo (withCompileInfo)
import           Pos.Util.Log.LoggerConfig (BackendKind (..), LogHandler (..),
                     LogSecurityLevel (..), LoggerConfig (..), LoggerTree (..),
                     RotationParameters (..))
import           Pos.Web.Types (TlsClientParams (..), TlsParams (..))


-- | Start a cluster of wallet nodes in different thread with the given NodeNames.
-- Node gets their argument from the ENV.
--
-- It returns a list of tuples, one for each node, where each tuple identify
-- a node with an async handle of the underlying running process.
-- Tuples also contains a 'TlsClientParams' record with path to TLS files needed
-- for a client to send HTTP request to the node.
startDemoCluster
    :: String     -- ^ A prefix. Only ENV vars with this prefix will be considered
    -> [NodeName] -- ^ A list of node names forming the cluster
    -> IO [(NodeName, TlsClientParams, Async ())]
startDemoCluster prefix nodes = do
    let indexedNodes = zip nodes (iterate (+1) 0)
    forM indexedNodes $ \(nodeId, i) -> do
        env <- (Map.fromList . mapMaybe filterMapEnv) <$> getEnvironment

        -- NOTE 'TlsClientParams' can't simply be returned by 'startDemoNode'
        -- because ultimately, this action never terminates and run as long as
        -- the node is running. Hence the 'MVar'.
        returnTLS <- newEmptyMVar
        handle <- async $ withStateDirectory env $ \stateDir -> do
            let ((initTopology, initLoggerConfig, initTLS), env') =
                    prepareDemoEnvironment env (nodeId, i) nodes stateDir
            initTopology >> initLoggerConfig >> initTLS >>= putMVar returnTLS
            startDemoNode nodeId env'
        tlsParams <- takeMVar returnTLS

        return (nodeId, tlsParams, handle)
  where
    -- | Only consider ENV var that have the given prefix
    filterMapEnv :: (String, String) -> Maybe (String, String)
    filterMapEnv (k, v) = (,)
        <$> stripPrefix prefix k
        <*> pure v


-- | Simple Type-Alias for readability. This mimics the actual ENV but in a pure
-- way. And bonus, we can use all sort of lenses and traversals on that!
type Env = Map String String


-- | Start a demo node (with wallet) using the given environment as a context.
-- This action never returns, unless the node crashes.
startDemoNode
    :: NodeName -- ^ The actual node name
    -> Env      -- ^ A "simulation" of the system environment as a 'Map String String'
    -> IO ()
startDemoNode (NodeName nodeIdT) env = do
    let cVars = varFromParser commonNodeArgsParser
    let nVars = varFromParser nodeArgsParser
    let wVars = varFromParser walletBackendParamsParser

    cArgs <- execParserEnv env cVars (info commonNodeArgsParser mempty)
    nArgs <- execParserEnv env nVars (info nodeArgsParser mempty)
    wOpts <- execParserEnv env wVars (info walletBackendParamsParser mempty)

    let wArgs = NewWalletBackendParams wOpts

    -- NOTE
    -- Logging to the console is disabled. This is just noise when multiple
    -- nodes are running at the same time. Logs are available in the logfiles
    -- inside the state directory anyway. `tail -f` is a friend.
    let lArgs = (loggingParams (fromString $ T.unpack nodeIdT) cArgs)
            { lpConsoleLog = Just False }

    withCompileInfo $ launchNode nArgs cArgs lArgs (actionWithWallet wArgs)


-- | Temporary Working / State Directory, use {prefix}STATE_DIR as a directory
-- if it's given, otherwise, create a new system-level temp directory.
withStateDirectory
    :: Env
    -> (FilePath -> IO a) -- ^ Action to run with the state directory
    -> IO a
withStateDirectory env cb =
    case env ^. at "STATE_DIR" of
        Nothing ->
            withSystemTempDirectory "cardano-sl-wallet:demo" cb

        Just dir ->
            cb dir


-- | Setup the environment for the node. This is where we assign default values
-- to mandatory arguments and constructs the related configurations (topology,
-- logging, tls, ...).
--
-- It returns actions that can be ran at a higher level to create and get those
-- configurations as well as a modified ENV which has been hydrated with
-- everything needed by the node to start.
prepareDemoEnvironment
    :: Env                  -- ^ ENVironment context with user-defined ENV vars
    -> (NodeName, Word16)   -- ^ Related node identifier
    -> [NodeName]           -- ^ All nodes, including the related one
    -> FilePath             -- ^ Node State / Working directory
    -> ((IO Topology, IO LoggerConfig, IO TlsClientParams), Env)
prepareDemoEnvironment baseEnv (NodeName nodeIdT, i) nodes stateDir =
    flip runState baseEnv $ modify withDefaultEnvironment >> (,,)
        <$> getsModify withNodeTopology
        <*> getsModify withNodeLoggerConfig
        <*> getsModify withNodeTLSCertificates
  where
    nodeId :: String
    nodeId = T.unpack nodeIdT

    withDefaultEnvironment :: Env -> Env
    withDefaultEnvironment env = env
        & at "CONFIGURATION_FILE" %~ (|> "../lib/configuration.yaml")
        & at "CONFIGURATION_KEY"  %~ (|> "default")
        & at "DB_PATH"            %~ (|> stateDir </> "db" </> nodeId)
        & at "LOG_SEVERITY"       %~ (|> "Debug")
        & at "NO_CLIENT_AUTH"     %~ (|> "False")
        & at "NODE_ID"            %~ (|> nodeId)
        & at "REBUILD_DB"         %~ (|> "True")
        & at "SYSTEM_START"       %~ (|> "0")
        & at "WALLET_DB_PATH"     %~ (|> stateDir </> "wallet-db" </> nodeId)
        & at "WALLET_REBUILD_DB"  %~ (|> "True")
        & at "LISTEN"             %~ nextStringAddrDef i ("127.0.0.1", 3000)
        & at "WALLET_ADDRESS"     %~ nextStringAddrDef i ("127.0.0.1", 8090)
        & at "WALLET_DOC_ADDRESS" %~ nextStringAddrDef i ("127.0.0.1", 8190)

    -- | Create the 'Topology' of the given node
    -- NOTE: The topology can't be overriden by ENV vars.
    withNodeTopology :: Env -> (IO Topology, Env)
    withNodeTopology env =
        let
            addr =
                -- NOTE Safe when called after 'withDefaultEnvironment'
                unsafeNetworkAddressFromString (env ! "LISTEN")

            topologyPath =
                stateDir </> "topology" </> T.unpack nodeIdT <> ".json"

            topology =
                demoTopology $ zip nodes (flip nextNtwrkAddr addr <$> (iterate (+1) 0))

            initTopology = do
                createDirectoryIfMissing True (takeDirectory topologyPath)
                BL.writeFile topologyPath (Aeson.encode topology)
                return topology
        in
            ( initTopology
            , env & at "TOPOLOGY" .~ Just topologyPath
            )

    -- | Create a 'LoggerConfig' for the given node
    -- NOTE: The 'LoggerConfig' can't be overriden by ENV vars, however,
    -- the severity can be adjusted with an extra env var 'LOG_SEVERITY'
    withNodeLoggerConfig :: Env -> (IO LoggerConfig, Env)
    withNodeLoggerConfig env =
        let
            loggerConfigPath =
                stateDir </> "logs" </> nodeId <> ".json"

            logFilePath =
                stateDir </> "logs" </> nodeId <> ".log"

            logSeverity =
                -- NOTE Safe when called after 'withDefaultEnvironment'
                unsafeSeverityFromString (env ! "LOG_SEVERITY")

            loggerConfig = LoggerConfig
                { _lcRotation = Just $ RotationParameters
                    { _rpLogLimitBytes = 104857600
                    , _rpMaxAgeHours   = 24
                    , _rpKeepFilesNum  = 1
                    }
                , _lcLoggerTree = LoggerTree
                    { _ltMinSeverity   = logSeverity
                    , _ltNamedSeverity = mempty
                    , _ltHandlers      = pure $ LogHandler
                        { _lhName          = toText nodeId
                        , _lhFpath         = Just logFilePath
                        , _lhBackend       = FileTextBE
                        , _lhMinSeverity   = Just logSeverity
                        , _lhSecurityLevel = Just SecretLogLevel
                        }

                    }
                , _lcBasePath = Nothing
                }

            initLoggerConfig = do
                createDirectoryIfMissing True (takeDirectory loggerConfigPath)
                BL.writeFile loggerConfigPath (Aeson.encode loggerConfig)
                return loggerConfig
        in
            ( initLoggerConfig
            , env & at "LOG_CONFIG" .~ Just loggerConfigPath
            )

    -- | Create TLS Certificates configurations
    -- NOTE: The TLS configurations & certs can't be overriden by ENV vars.
    withNodeTLSCertificates :: Env -> (IO TlsClientParams, Env)
    withNodeTLSCertificates env =
        let
            noClientAuth =
                -- NOTE Safe when called after 'withDefaultEnvironment'
                unsafeBoolFromString (env ! "NO_CLIENT_AUTH")

            tlsBasePath =
                stateDir </> "tls" </> nodeId

            tlsParams = TlsParams
                { tpCertPath   = tlsBasePath </> "server.crt"
                , tpKeyPath    = tlsBasePath </> "server.key"
                , tpCaPath     = tlsBasePath </> "ca.crt"
                , tpClientAuth = not noClientAuth
                }

            tlsClientParams = TlsClientParams
                { tpClientCertPath = tlsBasePath </> "client.crt"
                , tpClientKeyPath  = tlsBasePath </> "client.key"
                , tpClientCaPath   = tlsBasePath </> "ca.crt"
                }

            (tlsConf, dirConf) =
                demoTLSConfiguration tlsBasePath

            initTLSEnvironment = do
                keys <- genRSA256KeyPair
                let (ca, cs) = fromConfiguration tlsConf dirConf genRSA256KeyPair keys
                (_, caCert) <- genCertificate ca
                forM_ cs $ \c -> do
                    createDirectoryIfMissing True (certOutDir c)
                    writeCredentials (certOutDir c </> certFilename c) =<< genCertificate c
                    writeCertificate (certOutDir c </> certFilename ca) caCert
                return tlsClientParams
        in
            ( initTLSEnvironment
            , env
                & at "TLSCERT" ?~ tpCertPath tlsParams
                & at "TLSKEY"  ?~ tpKeyPath tlsParams
                & at "TLSCA"   ?~ tpCaPath tlsParams
            )


-- | Demo TLS Configuration
demoTLSConfiguration
    :: FilePath -- ^ Directory to output TLS stuff
    -> (TLSConfiguration, DirConfiguration)
demoTLSConfiguration dir =
    ( TLSConfiguration
        { tlsCa = CertConfiguration
            { certOrganization = "IOHK - Demo"
            , certCommonName   = "Root Self-Signed CA"
            , certExpiryDays   = 365
            }
        , tlsServer = ServerConfiguration
            { serverAltNames      = "localhost" :| [ "127.0.0.1" ]
            , serverConfiguration = CertConfiguration
                { certOrganization = "IOHK - Demo"
                , certCommonName   = "Server Certificate"
                , certExpiryDays   = 365
                }
            }
        , tlsClients = pure CertConfiguration
            { certOrganization = "IOHK - Demo"
            , certCommonName   = "Client Certificate"
            , certExpiryDays   = 365
            }
        }
    , DirConfiguration
        { outDirServer  = dir
        , outDirClients = dir
        , outDirCA      = Just dir
        }
    )


-- | Create a default topology file structure for the given nodes associated
-- with their corresponding network addresses
demoTopology
    :: [(NodeName, NetworkAddress)] -- ^ All fully-connected nodes
    -> Topology
demoTopology =
    TopologyStatic . AllStaticallyKnownPeers . Map.fromList . map mkPeer . subPermutations
  where
    mkPeer
        :: ((NodeName, NetworkAddress), [(NodeName, NetworkAddress)])
        -> (NodeName, NodeMetadata)
    mkPeer ((nodeId, addr), routes) =
        (nodeId, mkNodeMetadata addr (mkRoutes routes))

    mkRoutes
        :: [(NodeName, NetworkAddress)]
        -> NodeRoutes
    mkRoutes =
        NodeRoutes . map (pure . fst)

    mkNodeMetadata
        :: NetworkAddress
        -> NodeRoutes
        -> NodeMetadata
    mkNodeMetadata (addr, port) routes = NodeMetadata
        { nmType       = NodeCore
        , nmRegion     = NodeRegion "undefined"
        , nmRoutes     = routes
        , nmSubscribe  = DnsDomains []
        , nmValency    = 1
        , nmFallbacks  = 1
        , nmAddress    = NodeAddrExact (unsafeIPFromString $ B8.unpack addr) (Just port)
        , nmKademlia   = False
        , nmPublicDNS  = False
        , nmMaxSubscrs = BucketSizeUnlimited
        }

    subPermutations :: Eq a => [a] -> [(a, [a])]
    subPermutations xs =
        map (\ys -> (h ys, ys)) (sizedSubsequences (length xs - 1) xs)
      where
        h ys = Prelude.head (xs \\ ys)

    sizedSubsequences :: Int -> [a] -> [[a]]
    sizedSubsequences n =
        filter ((== n) . length) . subsequences


-- | Make HttpRequest continuously to wait after the node
waitForNode :: WalletClient IO -> IO ()
waitForNode wc = do
    resp <- getNodeInfo wc NoNtpCheck
    case resp of
        Right _ ->
            return ()

        Left (ClientHttpError ConnectionError{}) ->
            let
                oneSecond = 1000000
            in
                threadDelay oneSecond  >> waitForNode wc

        Left err ->
            fail $ "Failed to wait for node to start; the node has likely died: " <> show err


--
-- (Internal) 'NetworkAddress' manipulations
--

-- | Get the next 'NetworkAddress' given an index
nextNtwrkAddr :: Word16 -> NetworkAddress -> NetworkAddress
nextNtwrkAddr i (host, port) =
    (host, port + i)


-- | Get the next 'NextworkAddress' from a string, or from a default address otherwise
nextStringAddrDef :: Word16 -> NetworkAddress -> Maybe String -> Maybe String
nextStringAddrDef i defAddr str =
    (ntwrkAddrToString . nextNtwrkAddr i) <$>
        (fmap unsafeNetworkAddressFromString str <|> Just defAddr)


-- | Convert a 'NetworkAddress' to an ENV var
ntwrkAddrToString :: NetworkAddress -> String
ntwrkAddrToString (host, port) =
    B8.unpack host <> ":" <> show port



--
-- (Internal) ENV var <--> CLI args
--
-- We want to comprehend any ENVironment variables as if they were CLI
-- arguments and flags, such that we can re-use code to parse them and build
-- **Args structure from them.
--
-- The tricky thing here is that ENVironment "flags" don't exist so to speak,
-- so we have to reflect on the opt-applicative@Parser to see whether we expect
-- a var to be an 'Arg' or 'Flag'. If we expect a 'Flag' then the underlying
-- ENV var should be set to 'True' or 'False'
--
-- This also makes sure that we don't forget any CLI args and that's why, we
-- need to setup a full ENVironment before we actually start parsing. If one
-- variable is missing, it will throw a great deal.

data ArgType = Arg | Flag deriving Show

-- | Extract the list of ENV var from a 'Options.Applicative.Parser'
varFromParser :: Parser a -> [(String, ArgType)]
varFromParser parser =
    foldParse [] (helpToByteString help)
  where
    -- Here is the little trick, we leverage the parserFailure which displays
    -- a usage with all possible arguments and flags and from this usage,
    -- we capture all arguments and flags as tuple (String, ArgType)
    (help, _, _) =
        let
            pInfo = info parser mempty
        in
            execFailure (parserFailure Parser.defaultPrefs pInfo ShowHelpText mempty) ""

    -- NOTE: 'fromJust' is safe here as ther's always an usage
    helpToByteString :: ParserHelp -> ByteString
    helpToByteString =
        B8.pack . show . fromJust . unChunk . helpUsage

    -- | Convert a string argument to its corresponding ENV var
    argToVar :: String -> (String, ArgType)
    argToVar arg = case elemIndex ' ' arg of
        Nothing -> (kToS (drop 2 arg), Flag)
        Just i  -> (kToS (drop 2 (take i arg)), Arg)

    foldParse :: [(String, ArgType)] -> ByteString -> [(String, ArgType)]
    foldParse xs str = case parse (argToVar . B8.unpack <$> capture) str of
        Fail{}      -> xs
        Partial{}   -> xs
        Done rest x -> foldParse (x : xs) rest

    capture :: Atto.Internal.Parser ByteString ByteString
    capture =
        skipWhile (/= '[') *> string "[" *> takeWhile (/= ']') <* string "]"


-- | Run a parser from environment variables rather than command-line arguments
execParserEnv
    :: Env                 -- ^ The full environment ENV
    -> [(String, ArgType)] -- ^ The restricted variables to consider within theENV
    -> ParserInfo a        -- ^ A corresponding CLI
    -> IO a
execParserEnv env vars pInfo = do
    let args = mapMaybe (lookupEnv >=> varToArg) vars
    handleParseResult $ execParserPure Parser.defaultPrefs pInfo args
  where
    -- | Lookup a value at a given 'Key' in the environment and add it to
    -- the tuple ('Key', 'ArgType')
    lookupEnv :: (String, ArgType) -> Maybe (String, ArgType, String)
    lookupEnv (k, t) =
        (k, t, ) <$> (env ^. at k)

    -- | Convert an environment variable to its argument, with value. Returns
    -- 'Nothing' when Flags are given and turned off. 'Just arg' otherwise.
    varToArg :: (String, ArgType, String) -> Maybe String
    varToArg = \case
        (k, Flag, "True") -> Just ("--" <> sToK k)
        (_, Flag, _)      -> Nothing
        (k, Arg, v)       -> Just ("--" <> sToK k <> "=" <> v)

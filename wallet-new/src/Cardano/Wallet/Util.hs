-- | Module for small utility functions.
module Cardano.Wallet.Util
    ( -- * Miscellaneous
      (|>)
    , getsModify

    -- * String manipulation
    , headToLower
    , stripFieldPrefix
    , mkJsonKey
    , kToS
    , sToK

    -- * Unsafe Type-Coercion from String
    , unsafeIPFromString
    , unsafeBoolFromString
    , unsafeNetworkAddressFromString
    , unsafeSeverityFromString

    -- * Time
    , defaultApiTimeLocale
    , apiTimeFormat
    , parseApiUtcTime
    , showApiUtcTime
    ) where

import qualified Prelude
import           Universum

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B8
import           Data.Char (isUpper, toLower)
import qualified Data.Char as Char
import           Data.IP (IP)
import qualified Data.Time as Time
import           Formatting (bprint, build, formatToString, (%))
import qualified Formatting.Buildable
import qualified Safe
import qualified Text.Parsec as Parsec

import           Pos.Core.NetworkAddress (NetworkAddress, addrParser)
import           Pos.Util.Log.Severity (Severity (..))


-- * Miscellaneous

-- | Provide a default value for an Alternative
(|>) :: Alternative f => f a -> a -> f a
f |> a = f <|> pure a
infix 4 |>

-- | Apply a function using the current state that returns a value and modifies
-- the state.
getsModify :: (s -> (a, s)) -> State s a
getsModify f = do
    (a, s') <- gets f
    put s' >> return a


-- * String manipulation utils

headToLower :: String -> Maybe String
headToLower []     = Nothing
headToLower (x:xs) = Just $ toLower x : xs

stripFieldPrefix :: String -> String
stripFieldPrefix = dropWhile (not . isUpper)

mkJsonKey :: String -> String
mkJsonKey s = fromMaybe s . headToLower $ stripFieldPrefix s

-- | kebab-case to UPPER_SNAKE_CASE
kToS :: String -> String
kToS = map (Char.toUpper . replaceIf '-' '_')

-- | UPPER_SNAKE_CASE to kebab-case
sToK :: String -> String
sToK = map (Char.toLower . replaceIf '_' '-')

-- | Replace third argument by the second one if it matches the first one.
replaceIf :: Char -> Char -> Char -> Char
replaceIf want to x | x == want = to
replaceIf _ _ x     = x


-- * Unsafe Type-Coercion from string

-- | Parse a 'ByteString' into an 'IP'.
unsafeIPFromString :: String -> IP
unsafeIPFromString bytes =
    case Safe.readMay bytes of
        Nothing ->
            error $ "'unsafeIPFromBS' was given a malformed value that can't\
                \ be parsed to an IP: " <> toText bytes
        Just ip ->
            ip

unsafeBoolFromString :: String -> Bool
unsafeBoolFromString str =
    case Safe.readMay str of
        Nothing ->
            error $ "Tried to convert a 'String' to a 'Bool' but failed: " <> toText str
        Just b ->
            b

unsafeNetworkAddressFromString :: String -> NetworkAddress
unsafeNetworkAddressFromString str =
    case Parsec.parse addrParser "" (toText str) of
        Left err ->
            error $ "Tried to convert a 'String' to a 'NetworkAddress' but failed: "
                <> toText str <> ", " <> show err
        Right addr ->
            addr

unsafeSeverityFromString :: String -> Severity
unsafeSeverityFromString str =
    case Aeson.decodeStrict (B8.pack str) of
        Nothing ->
            error $ "Tried to convert a 'String' to a 'Severity' but failed."

        Just severity ->
            severity


-- * Time

-- | Currently we support only American usage.
defaultApiTimeLocale :: Time.TimeLocale
defaultApiTimeLocale = Time.defaultTimeLocale

-- | Time format used in API. Corresponds to ISO-8601.
-- Example of time: "2018-03-07T16:20:27.477318"
--
-- Note: there is more neat support of time formats in time-1.9,
-- Data.Time.Format.ISO8601 module, but that version is barely applicable with
-- current LTS-9.1.
apiTimeFormat :: String
apiTimeFormat = Time.iso8601DateFormat (Just "%H:%M:%S%Q")

newtype UtcTimeParseError = UtcTimeParseError Text

instance Buildable UtcTimeParseError where
    build (UtcTimeParseError msg) = bprint ("UTC time parse error: "%build) msg

instance Show UtcTimeParseError where
    show = formatToString build

-- | Parse UTC time from API.
parseApiUtcTime :: Text -> Either UtcTimeParseError Time.UTCTime
parseApiUtcTime =
    first UtcTimeParseError .
    Time.parseTimeM False defaultApiTimeLocale apiTimeFormat .
    toString

-- | Encode UTC time for API.
showApiUtcTime :: Time.UTCTime -> Text
showApiUtcTime = toText . Time.formatTime defaultApiTimeLocale apiTimeFormat

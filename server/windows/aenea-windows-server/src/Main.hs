{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Main (main) where

import Windows( Key, Direction (..)
              , nameToKey, charToKey
              , keyPress, keyAction
              , withKeyPress
              , getForegroundWindowText
              , getForegroundWindowAncestorText)
import Network.JsonRpc.Server( Parameter (..)
                             , (:+:) (..)
                             , RpcResult
                             , call
                             , Method
                             , toMethod
                             , rpcError)
import Data.Bits (xor, (.|.))
import Data.Char (ord)
import Data.Function (on)
import Data.Text (Text, unpack, append)
import Data.String (fromString)
import Data.List (intersperse)
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Data.Aeson (FromJSON (parseJSON), Value (Number, String), object, withText, (.=))
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBChar8
import Control.Monad ((<=<), when, forM_)
import Control.Monad.Except (mapExceptT)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ReaderT(..), ask, lift, runReaderT)
import Control.Concurrent (threadDelay, readMVar)
import Happstack.Lite (Request, toResponse)
import qualified Happstack.Server.SimpleHTTP as H
import Happstack.Server.Internal.Types (noContentLength)
import System.Environment (getArgs)
import System.Console.GetOpt( OptDescr (Option), ArgDescr(..)
                            , ArgOrder (Permute), getOpt, usageInfo)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

#if MIN_VERSION_mtl(2,2,1)
import Control.Monad.Except (throwError)
#else
import Control.Monad.Error (throwError)
#endif

newtype SecurityToken = SecurityToken String

instance FromJSON SecurityToken where
  parseJSON = withText "SecurityToken" (return . SecurityToken . unpack)

-- Monad used by all RPC methods on the server.
type Server = ReaderT SecurityToken IO

main :: IO ()
main = getArgs >>= \args ->
  case parseArgs args of
    Left Nothing -> putStrLn usage >> exitSuccess
    Left (Just errs) -> putStrLn (errs ++ "\r\n" ++ usage) >> exitWith (ExitFailure 1)
    Right (maybeAddress, portStr, token, verbosity) ->
        case parseInt portStr of
          Nothing -> putStrLn "cannot parse port" >> exitWith (ExitFailure 1)
          Just port -> serve verbosity maybeAddress port token

serve :: Verbosity -> Maybe Address -> Int -> SecurityToken -> IO ()
serve verbosity maybeAddress port token =
    let conf = H.nullConf {H.port = port}
    in case maybeAddress of
         Nothing -> putStrLn ("listening on port " ++ show port) >>
                    H.simpleHTTP conf (handleRequests token verbosity)
         Just addr -> do
           s <- H.bindIPv4 addr port
           putStrLn $ "listening on " ++ addr ++ ":" ++ show port
           H.simpleHTTPWithSocket s conf $ handleRequests token verbosity

handleRequests :: SecurityToken -> Verbosity -> H.ServerPartT IO H.Response
handleRequests token verbosity = do
  request <- H.askRq
  body <- lift $ getBody request
  printMsg "" >> printMsg body
  result <- lift $ runReaderT (call methods body) token
  let resultStr = fromMaybe "" result
  printMsg resultStr
  return $ noContentLength $ toResponse resultStr
    where printMsg = when (verbosity == Verbose)
                     . lift . LBChar8.putStrLn

getBody :: Request -> IO LB.ByteString
getBody r = H.unBody <$> readMVar (H.rqBody r)

usage :: String
usage = usageInfo "usage: aenea [options]" options

data Verbosity = Verbose | Quiet deriving Eq

type Address = String
type Port = String

parseArgs :: [String] -> Either (Maybe String) (Maybe Address, Port, SecurityToken, Verbosity)
parseArgs args = case getOpt Permute options args of
                   (opts, _, []) | Help `elem` opts -> Left Nothing
                   (opts, [], []) -> parseOpts opts
                   (_, _, []) -> Left $ Just msg
                   (_, _, errs) -> Left $ Just $ concat errs
                 where msg = "unexpected arguments\n"
                       parseOpts opts =
                           let addr = getArg opts getAddress Nothing
                               port = getArg opts getPort "8240"
                               mToken = getArg opts getSecuityToken Nothing
                               verbosity = getArg opts getVerbosity Quiet
                           in case mToken of
                                Nothing -> Left (Just "missing required security token flag")
                                Just token -> Right (addr, port, token, verbosity)

getArg :: [Flag] -> (Flag -> Maybe a) -> a -> a
getArg opts f default' = case safeLast $ mapMaybe f opts of
                           Just arg -> arg
                           Nothing -> default'

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

getAddress :: Flag -> Maybe (Maybe Address)
getAddress (AddressFlag a) = Just $ Just a
getAddress _ = Nothing

getPort :: Flag -> Maybe Port
getPort (PortFlag p) = Just p
getPort _ = Nothing

getSecuityToken :: Flag -> Maybe (Maybe SecurityToken)
getSecuityToken (SecurityTokenFlag t) = Just $ Just (SecurityToken t)
getSecuityToken _ = Nothing

getVerbosity :: Flag -> Maybe Verbosity
getVerbosity Verbosity = Just Verbose
getVerbosity _ = Nothing

parseInt :: String -> Maybe Int
parseInt x = case reads x of
               [(i, [])] -> Just i
               _ -> Nothing

data Flag = AddressFlag String | PortFlag String | SecurityTokenFlag String | Verbosity | Help
            deriving (Eq, Ord, Show)

options :: [OptDescr Flag]
options = [ Option "a" ["address"] (ReqArg AddressFlag "ADDRESS") "specify host IP address (not host name)"
          , Option "p" ["port"] (ReqArg PortFlag "PORT") "specify host port (default is 8240)"
          , Option "t"
                   ["security-token"]
                   (ReqArg SecurityTokenFlag "STRING")
                   "specify the security token (required)"
          , Option "v" ["verbose"] (NoArg Verbosity) "print JSON-RPC requests and responses"
          , Option "h" ["help"] (NoArg Help) "show this message"]

securityTokenKey :: Text
securityTokenKey = "security_token"

-- All methods must call 'withSecurityTokenValidation'.
methods :: [Method Server]
methods = [ getContextMethod
          , keyPressMethod
          , writeTextMethod
          , pauseMethod
          , serverInfoMethod ]

keyPressMethod :: Method Server
keyPressMethod = toMethod "key_press" keyPressFunction
                 (Required securityTokenKey :+:
                  Required "key" :+:
                  Optional "modifiers" [] :+:
                  Optional "direction" Press :+:
                  Optional "count" 1 :+:
                  Optional "delay" defaultKeyDelay :+: ())

keyPressFunction :: SecurityToken -> Text -> [Text] -> Direction -> Int -> Int -> RpcResult Server ()
keyPressFunction token keyName modifiers direction count delayMillis =
    withSecurityTokenValidation token $ do
      key <- tryLookupKey nameToKey id keyName
      modKeys <- mapM (tryLookupKey nameToKey id) modifiers
      let withPress k task = withKeyPress k (delay >> task >> delay)
          pressKey = sequence_ $ intersperse delay $
                       replicate count $ keyAction direction key
          delay = threadDelay millis
          millis = if delayMillis >= 0 then delayMillis else defaultKeyDelay
      lift $ compose (withPress <$> modKeys) pressKey
        where compose = foldl (.) id

defaultKeyDelay :: Int
defaultKeyDelay = -1

getContextMethod :: Method Server
getContextMethod = toMethod "get_context" context (Required securityTokenKey :+: ())
    where context :: SecurityToken -> RpcResult Server Value
          context token =
              withSecurityTokenValidation token $
              liftIO $ (object . concat . catMaybes) <$> sequence [ancestor, active]
          ancestor = ((\t -> ["name" .= t]) <$>) <$> getForegroundWindowAncestorText
          active = (titlePair <$>) <$> getForegroundWindowText
          titlePair text = ["id" .= ("" :: String), "title" .= text]

serverInfoMethod :: Method Server
serverInfoMethod = toMethod "server_info" serverInfo (Required securityTokenKey :+: ())
    where serverInfo :: SecurityToken -> RpcResult Server Value
          serverInfo token =
              withSecurityTokenValidation token $
              return $ object [ "window_manager" .= String "windows"
                              , "operating_system" .= String "windows"
                              , "platform" .= String "windows"
                              , "display" .= String "windows"
                              , "server" .= String "aenea"
                              , "server_version" .= Number 1
                              ]

writeTextMethod :: Method Server
writeTextMethod = toMethod "write_text" writeTextFunction
                  (Required securityTokenKey :+: Required "text" :+: ())

writeTextFunction :: SecurityToken -> Text -> RpcResult Server ()
writeTextFunction token text =
    withSecurityTokenValidation token $
    forM_ (unpack text) $
    lift . keyPress <=< tryLookupKey charToKey charToText
  where
    charToText c = fromString [c]

pauseMethod :: Method Server
pauseMethod = toMethod "pause" pause (Required securityTokenKey :+: Required "amount" :+: ())
    where pause :: SecurityToken -> Int -> RpcResult Server ()
          pause token ms =
              withSecurityTokenValidation token $
              liftIO $ threadDelay (1000 * ms)

withSecurityTokenValidation :: SecurityToken -> RpcResult IO a -> RpcResult Server a
withSecurityTokenValidation actualToken result = do
  expectedToken <- ask
  if constantTimeEquals expectedToken actualToken
    then mapExceptT (ReaderT . const) result
    else throwError $ rpcError 1 "permission denied"

constantTimeEquals :: SecurityToken -> SecurityToken -> Bool
constantTimeEquals (SecurityToken expected) (SecurityToken actual) =
  if length expected /= length actual
  then False
  else let result = foldr (.|.) 0 $ zipWith (xor `on` ord) expected actual
       in result == 0

tryLookupKey :: (a -> Maybe Key) -> (a -> Text) -> a -> RpcResult IO Key
tryLookupKey f toText k = case f k of
                              Nothing -> throwError $ keyNotFound $ toText k
                              Just key -> return key
    where keyNotFound key = rpcError 32000 $ "Cannot find key: " `append` key

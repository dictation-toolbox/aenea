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
import Data.Text (Text, unpack, append)
import Data.String (fromString)
import Data.List (intersperse)
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Data.Aeson (Value (Number, String), object, (.=))
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBChar8
import Control.Monad ((<=<), when, forM_)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (lift)
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

main :: IO ()
main = getArgs >>= \args ->
  case parseArgs args of
    Left Nothing -> putStrLn usage >> exitSuccess
    Left (Just errs) -> putStrLn (errs ++ usage) >> exitWith (ExitFailure 1)
    Right (maybeAddress, portStr, verbosity) ->
        case parseInt portStr of
          Nothing -> putStrLn "cannot parse port" >> exitWith (ExitFailure 1)
          Just port -> serve verbosity maybeAddress port

serve :: Verbosity -> Maybe Address -> Int -> IO ()
serve verbosity maybeAddress port =
    let conf = H.nullConf {H.port = port}
    in case maybeAddress of
         Nothing -> putStrLn ("listening on port " ++ show port) >>
                    H.simpleHTTP conf (handleRequests verbosity)
         Just addr -> do
           s <- H.bindIPv4 addr port
           putStrLn $ "listening on " ++ addr ++ ":" ++ show port
           H.simpleHTTPWithSocket s conf $ handleRequests verbosity

handleRequests :: Verbosity -> H.ServerPartT IO H.Response
handleRequests verbosity = do
  request <- H.askRq
  body <- lift $ getBody request
  printMsg "" >> printMsg body
  result <- lift $ call methods body
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

parseArgs :: [String] -> Either (Maybe String) (Maybe Address, Port, Verbosity)
parseArgs args = case getOpt Permute options args of
                   (opts, _, []) | Help `elem` opts -> Left Nothing
                   (opts, [], []) -> Right $ parseOpts opts
                   (_, _, []) -> Left $ Just msg
                   (_, _, errs) -> Left $ Just $ concat errs
                 where msg = "unexpected arguments\n"
                       parseOpts opts =
                           let addr = getArg opts getAddress Nothing
                               port = getArg opts getPort "8240"
                               verbosity = getArg opts getVerbosity Quiet
                           in (addr, port, verbosity)

getArg :: [Flag] -> (Flag -> Maybe a) -> a -> a
getArg opts f default' = case safeLast $ mapMaybe f opts of
                           Just arg -> arg
                           Nothing -> default'

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

getAddress :: Flag -> Maybe (Maybe Address)
getAddress (Address a) = Just $ Just a
getAddress _ = Nothing

getPort :: Flag -> Maybe Port
getPort (Port p) = Just p
getPort _ = Nothing

getVerbosity :: Flag -> Maybe Verbosity
getVerbosity Verbosity = Just Verbose
getVerbosity _ = Nothing

parseInt :: String -> Maybe Int
parseInt x = case reads x of
               [(i, [])] -> Just i
               _ -> Nothing

data Flag = Address String | Port String | Verbosity | Help
            deriving (Eq, Ord, Show)

options :: [OptDescr Flag]
options = [ Option "a" ["address"] (ReqArg Address "address") "specify host IP address (not host name)"
          , Option "p" ["port"] (ReqArg Port "port") "specify host port (default is 8240)"
          , Option "v" ["verbose"] (NoArg Verbosity) "print JSON-RPC requests and responses"
          , Option "h" ["help"] (NoArg Help) "show this message"]

methods :: [Method IO]
methods = [ getContextMethod
          , keyPressMethod
          , writeTextMethod
          , pauseMethod
          , serverInfoMethod ]

keyPressMethod :: Method IO
keyPressMethod = toMethod "key_press" keyPressFunction
                 (Required "key" :+:
                  Optional "modifiers" [] :+:
                  Optional "direction" Press :+:
                  Optional "count" 1 :+:
                  Optional "delay" defaultKeyDelay :+: ())

keyPressFunction :: Text -> [Text] -> Direction -> Int -> Int -> RpcResult IO ()
keyPressFunction keyName modifiers direction count delayMillis = do
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

getContextMethod :: Method IO
getContextMethod = toMethod "get_context" context ()
    where context :: RpcResult IO Value
          context = liftIO $ (object . concat . catMaybes) <$> sequence [ancestor, active]
          ancestor = ((\t -> ["name" .= t]) <$>) <$> getForegroundWindowAncestorText
          active = (titlePair <$>) <$> getForegroundWindowText
          titlePair text = ["id" .= ("" :: String), "title" .= text]

serverInfoMethod :: Method IO
serverInfoMethod = toMethod "server_info" serverInfo ()
    where serverInfo :: RpcResult IO Value
          serverInfo = return $ object
                         [ "window_manager" .= String "windows"
                         , "operating_system" .= String "windows"
                         , "platform" .= String "windows"
                         , "display" .= String "windows"
                         , "server" .= String "aenea"
                         , "server_version" .= Number 1
                         ]

writeTextMethod :: Method IO
writeTextMethod = toMethod "write_text" writeTextFunction
                  (Required "text" :+: ())

writeTextFunction :: Text -> RpcResult IO ()
writeTextFunction text = forM_ (unpack text) $
                         lift . keyPress <=< tryLookupKey charToKey charToText
    where charToText c = fromString [c]

pauseMethod :: Method IO
pauseMethod = toMethod "pause" pause (Required "amount" :+: ())
    where pause :: Int -> RpcResult IO ()
          pause ms = liftIO $ threadDelay (1000 * ms)

tryLookupKey :: (a -> Maybe Key) -> (a -> Text) -> a -> RpcResult IO Key
tryLookupKey f toText k = case f k of
                              Nothing -> throwError $ keyNotFound $ toText k
                              Just key -> return key
    where keyNotFound key = rpcError 32000 $ "Cannot find key: " `append` key

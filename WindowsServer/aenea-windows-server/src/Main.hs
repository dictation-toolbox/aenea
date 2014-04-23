{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Windows
import Network.JsonRpc.Server( Parameter (..)
                             , (:+:) (..)
                             , RpcResult
                             , call
                             , Method
                             , toMethod
                             , toMethods
                             , rpcError)
import Data.Text (Text, unpack, append)
import Data.String (fromString)
import Data.List (delete, intersperse)
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Data.Aeson (Value, object, (.=))
import qualified Data.ByteString.Lazy as B
import Control.Applicative ((<$>))
import Control.Monad ((<=<), guard, forM_)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (lift)
import Control.Monad.Error (throwError)
import Control.Concurrent (threadDelay, readMVar)
import Happstack.Lite (Request, toResponse)
import qualified Happstack.Server.SimpleHTTP as H
import Happstack.Server.Internal.Types (noContentLength)
import System.Environment (getArgs)
import System.Console.GetOpt( OptDescr (Option), ArgDescr(..)
                            , ArgOrder (Permute), getOpt, usageInfo)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)

main :: IO ()
main = getArgs >>= \args ->
  case parseArgs args of
    Left Nothing -> putStrLn usage >> exitSuccess
    Left (Just errs) -> putStrLn (errs ++ usage) >> exitWith (ExitFailure 1)
    Right (maybeAddress, portStr) -> case parseInt portStr of
                                       Nothing -> putStrLn "cannot parse port" >> exitWith (ExitFailure 1)
                                       Just port -> serve maybeAddress port

serve :: Maybe String -> Int -> IO ()
serve maybeAddress port = let conf = H.nullConf {H.port = port}
                          in case maybeAddress of
                               Nothing -> putStrLn ("listening on port " ++ show port) >>
                                          H.simpleHTTP conf handleRequests
                               Just addr -> do
                                 s <- H.bindIPv4 addr port
                                 putStrLn $ "listening on " ++ addr ++ ":" ++ show port
                                 H.simpleHTTPWithSocket s conf handleRequests

handleRequests :: H.ServerPartT IO H.Response
handleRequests = do
  request <- H.askRq
  body <- lift $ getBody request
  result <- lift $ call (toMethods methods) body
  let resultStr = fromMaybe "" result
      response = toResponse resultStr
  return $ noContentLength response

getBody :: Request -> IO B.ByteString
getBody r = H.unBody <$> readMVar (H.rqBody r)

usage :: String
usage = usageInfo "usage: aenea [options]" options

parseArgs :: [String] -> Either (Maybe String) (Maybe String, String)
parseArgs args = case getOpt Permute options args of
                   (opts, _, []) | Help `elem` opts -> Left Nothing
                   (opts, [], []) -> maybe (Left $ Just msg) Right $ parseOpts opts
                   (_, _, []) -> Left $ Just msg
                   (_, _, errs) -> Left $ Just $ concat errs
                 where msg = "unexpected arguments\n"
                       parseOpts opts = do
                         (addr, opts') <- getArg opts getAddress Nothing
                         (port, opts'') <- getArg opts' getPort "8240"
                         guard $ null opts''
                         return (addr, port)

getArg :: [Flag] -> (Flag -> Maybe (Flag, a)) -> a -> Maybe (a, [Flag])
getArg opts f default' = case mapMaybe f opts of
                           [(flag, arg)] -> Just (arg, delete flag opts)
                           [] -> Just (default', opts)
                           _ -> Nothing

getAddress x@(Address a) = Just (x, Just a)
getAddress _ = Nothing

getPort x@(Port p) = Just (x, p)
getPort _ = Nothing

parseInt :: String -> Maybe Int
parseInt x = case reads x of
               [(i, [])] -> Just i
               _ -> Nothing

data Flag = Address String | Port String | Help
            deriving (Eq, Ord, Show)

options :: [OptDescr Flag]
options = [ Option "a" ["address"] (ReqArg Address "address") "specify host IP address (not host name)"
          , Option "p" ["port"] (ReqArg Port "port") "specify host port (default is 8240)"
          , Option "h" ["help"] (NoArg Help) "show this message"]

methods = [ getContextMethod
          , keyPressMethod
          , writeTextMethod
          , pauseMethod ]

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

defaultKeyDelay = -1

getContextMethod :: Method IO
getContextMethod = toMethod "get_context" context ()
    where context :: RpcResult IO Value
          context = liftIO $ (object . concat . catMaybes) <$> sequence [ancestor, active]
          ancestor = ((\t -> ["name" .= t]) <$>) <$> getForegroundWindowAncestorText
          active = (titlePair <$>) <$> getForegroundWindowText
          titlePair text = ["id" .= ("" :: String), "title" .= text]

writeTextMethod = toMethod "write_text" writeTextFunction
                  (Required "text" :+: ())

writeTextFunction :: Text -> RpcResult IO ()
writeTextFunction text = forM_ (unpack text) $
                         lift . keyPress <=< tryLookupKey charToKey charToText
    where charToText c = fromString [c]

pauseMethod = toMethod "pause" pause (Required "amount" :+: ())
    where pause :: Int -> RpcResult IO ()
          pause ms = liftIO $ threadDelay (1000 * ms)

tryLookupKey :: (a -> Maybe Key) -> (a -> Text) -> a -> RpcResult IO Key
tryLookupKey f toText k = case f k of
                              Nothing -> throwError $ keyNotFound $ toText k
                              Just key -> return key
    where keyNotFound key = rpcError 32000 $ "Cannot find key: " `append` key

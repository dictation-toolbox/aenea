{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Windows
import Data.JsonRpc.Server( Param (..)
                          , RpcResult
                          , call
                          , toJsonFunction
                          , toJsonFunctions
                          , rpcError
                          , liftToResult)
import Data.Text (Text, unpack, append)
import Data.String (fromString)
import Data.List (intersperse)
import Data.Maybe (isNothing, catMaybes)
import Data.Aeson (object, (.=))
import qualified Data.ByteString.Lazy as B
import Control.Applicative ((<$>))
import Control.Monad (when, forM_)
import Control.Monad.Reader (lift)
import Control.Monad.Error (throwError)
import Control.Concurrent (threadDelay, readMVar)
import Happstack.Lite (Request, toResponse)
import Happstack.Server.SimpleHTTP( nullConf
                                  , bindIPv4
                                  , port
                                  , simpleHTTPWithSocket
                                  , askRq
                                  , rqBody
                                  , unBody)
import Happstack.Server.Internal.Types (noContentLength)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) $ exitWithError "expecting IP address and port"
  let address = args !! 0
      maybePort = parseInt $ args !! 1
  when (isNothing maybePort) $
       exitWithError "expecting IP address and port, cannot parse port"
  let Just port' = maybePort
  s <- bindIPv4 address port'
  simpleHTTPWithSocket s (nullConf {port = port'}) $ do
         request <- askRq
         body <- lift $ getBody request
         result <- lift $ call (toJsonFunctions methods) body
         let resultStr = maybe "" id result
             response = toResponse resultStr
         return $ noContentLength response

exitWithError :: String -> IO ()
exitWithError msg = putStrLn msg >> exitWith (ExitFailure 1)

parseInt :: String -> Maybe Int
parseInt x = case reads x of
               [(i, _)] -> Just i
               _ -> Nothing

getBody :: Request -> IO B.ByteString
getBody r = unBody <$> (readMVar $ rqBody r)

methods = [ getContextMethod
          , keyPressMethod
          , writeTextMethod
          , pauseMethod ]

keyPressMethod = toJsonFunction "key_press" keyPressFunction
           (Param "key" Nothing,
            (Param "modifiers" (Just []),
             (Param "direction" (Just Press),
              (Param "count" (Just 1),
               (Param "delay" (Just (defaultKeyDelay)), ())))))

keyPressFunction :: Text -> [Text] -> Direction -> Int -> Int -> RpcResult IO ()
keyPressFunction keyName modifiers direction count delayMillis = do
  key <- tryLookupKey nameToKey id keyName
  modKeys <- mapM (tryLookupKey nameToKey id) modifiers
  let keyActions = modsDown ++ keyPresses ++ modsUp
      modsDown = map keyDown modKeys
      modsUp = map keyUp modKeys
      keyPresses = replicate count $ keyAction direction key
      delay = threadDelay millis
      millis = if delayMillis >= 0 then delayMillis else defaultKeyDelay
  lift $ sequence_ $ intersperse delay keyActions

defaultKeyDelay = (-1)

getContextMethod = toJsonFunction "get_context" (liftToResult $ context) ()
    where context = (object . concat . catMaybes) <$> sequence [ancestor, active]
          ancestor = ((\t -> ["name" .= t]) <$>) <$> getForegroundWindowAncestorText
          active = (titlePair <$>) <$> getForegroundWindowText
          titlePair text = ["id" .= ("" :: String), "title" .= text]

writeTextMethod = toJsonFunction "write_text" writeTextFunction
                  (Param "text" Nothing, ())

writeTextFunction :: Text -> RpcResult IO ()
writeTextFunction text = forM_ (unpack text) $ \k ->
                         tryLookupKey charToKey charToText k >>= lift . keyPress
                         where charToText = fromString . (:[])

pauseMethod = toJsonFunction "pause" (\millis -> liftToResult $ threadDelay (1000 * millis))
              (Param "amount" Nothing, ())

tryLookupKey :: (a -> Maybe Key) -> (a -> Text) -> a -> RpcResult IO Key
tryLookupKey f toText k = case f k of
                              Nothing -> throwError $ keyNotFound $ toText k
                              Just key -> return key
    where keyNotFound key = rpcError 32000 $ "Cannot find key: " `append` key

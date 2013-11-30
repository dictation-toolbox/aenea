{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main (main) where

import WindowsKeys
import JsonRpcServer
import Data.Maybe
import Data.Text (Text, unpack, append)
import Data.String (fromString)
import Control.Applicative ((<$>))
import Control.Monad (forM_, replicateM_)
import Control.Monad.Reader (lift, liftIO)
import Control.Monad.Error (throwError)
import Control.Concurrent (threadDelay, readMVar)
import Happstack.Lite (Request, toResponse)
import Happstack.Server.SimpleHTTP (nullConf, port, simpleHTTP, askRq, rqBody, unBody)
import qualified Data.ByteString.Lazy as B
import Data.Aeson (object, (.=))

main :: IO ()
main = simpleHTTP (nullConf {port = 8240}) $ do
         r <- askRq
--         liftIO $ print r
         body <- liftIO $ getBody r
--         liftIO $ print body
         r2 <- lift $ call (toJsonFunctions methods) body
         let r3 = maybe "" id r2
         return $ toResponse $ r3

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
  case nameToKey keyName of
    Nothing -> throwError $ keyNotFound keyName
    Just key -> liftIO $ do
                  forM_ modKeys (\k -> keyDown k >> delay)
                  replicateM_ count $ (keyAction direction key >> delay)
                  forM_ modKeys (\k -> keyUp k >> delay)
                      where modKeys = map (fromJust . nameToKey) modifiers
                            delay = threadDelay millis
                            millis = if delayMillis >= 0 then delayMillis else defaultKeyDelay

defaultKeyDelay = -1

getContextMethod = toJsonFunction "get_context" (liftR $ return $ defaultContext) ()
    where defaultContext = object ["id" .= emptyStr, "title" .= emptyStr]
          emptyStr = "" :: String

writeTextMethod = toJsonFunction "write_text" writeTextFunction
                  (Param "text" Nothing, ())

writeTextFunction :: Text -> RpcResult IO ()
writeTextFunction text = forM_ (unpack text) $ \k -> do
                           case charToKey k of
                             Nothing -> throwError $ keyNotFound $ fromString [k]
                             Just key -> liftIO $ keyPress key

pauseMethod = toJsonFunction "pause" (\millis -> liftR $ threadDelay (1000 * millis))
              (Param "amount" Nothing, ())

keyNotFound :: Text -> RpcError
keyNotFound key = rpcError 32000 $ "Cannot find key: " `append` key

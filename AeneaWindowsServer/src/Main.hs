{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main (main) where

import WindowsKeys
import Data.JsonRpc.Server
import Data.Text (Text, unpack, append)
import Data.String (fromString)
import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.Reader (lift, liftIO)
import Control.Monad.Error (throwError)
import Control.Concurrent (threadDelay, readMVar)
import Happstack.Lite (Request, toResponse)
import Happstack.Server.SimpleHTTP (nullConf, port, simpleHTTP, askRq, rqBody, unBody)
import Happstack.Server.Internal.Types (noContentLength)
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
         let r4 = toResponse r3
--         liftIO $ print r4
         let r5 = noContentLength r4
--         liftIO $ print r5
         return r5

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
  liftIO $ sequence_ $ intersperse delay keyActions

defaultKeyDelay = -1

getContextMethod = toJsonFunction "get_context" (liftToResult $ context) ()
    where context = (object . concat . catMaybes) <$> sequence [ancestor, active]
          ancestor = ((\t -> ["name" .= t]) <$>) <$> getForegroundWindowAncestorText
          active = (titlePair <$>) <$> getForegroundWindowText
          titlePair text = ["id" .= ("" :: String), "title" .= text]

writeTextMethod = toJsonFunction "write_text" writeTextFunction
                  (Param "text" Nothing, ())

writeTextFunction :: Text -> RpcResult IO ()
writeTextFunction text = forM_ (unpack text) $ \k ->
                         tryLookupKey charToKey charToText k >>= liftIO . keyPress
                         where charToText = fromString . (:[])

pauseMethod = toJsonFunction "pause" (\millis -> liftToResult $ threadDelay (1000 * millis))
              (Param "amount" Nothing, ())

tryLookupKey :: (a -> Maybe Key) -> (a -> Text) -> a -> RpcResult IO Key
tryLookupKey f toText k = case f k of
                              Nothing -> throwError $ keyNotFound $ toText k
                              Just key -> return key
    where keyNotFound key = rpcError 32000 $ "Cannot find key: " `append` key

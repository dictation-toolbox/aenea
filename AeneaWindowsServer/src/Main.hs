{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main (main) where

import WindowsKeys
import JsonRpcServer
import Data.Maybe
import Data.Text hiding (map)
import Data.List (nub)
import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Concurrent
import Control.Concurrent.MVar
import Happstack.Lite hiding (port, serve, method)
import Happstack.Server.SimpleHTTP hiding (method)
import Happstack.Server.Internal.Monads
import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as J
import System.IO
import Data.Aeson

main :: IO ()
main = simpleHTTP (nullConf {port = 8240}) $ do
         r <- askRq
--         liftIO $ print r
         body <- liftIO $ getBody r
--         liftIO $ print b
         r2 <- lift $ call methods body
         let r3 = maybe "" id r2
         return $ toResponse $ r3

getBody :: Request -> IO B.ByteString
getBody r = unBody <$> (readMVar $ rqBody r)

methods = toJsonFunctions [getContext, keyPressMethod, writeText, pause]

keyPressMethod = toJsonFunction "key_press" (\k ms d c d' -> liftR (keyPressF k ms d c d'))
           (Param "key" Nothing,
            (Param "modifiers" (Just []),
             (Param "direction" (Just Press),
              (Param "count" (Just 1),
               (Param "delay" (Just (defaultKeyDelay)), ())))))

keyPressF :: Text -> [Text] -> Direction -> Int -> Int -> IO ()
keyPressF key modifiers direction count delayMillis = do
  print (keyNames key')
  forM_ allModKeys (\k -> keyDown k >> delay)
  replicateM_ count $ (keyEvent direction key' >> delay)
  forM_ allModKeys (\k -> keyUp k >> delay)
    where Just key' = nameToKey key
          modKeys = map (fromJust . nameToKey) modifiers
          allModKeys = if keyRequiresShift key'
                       then nub (key_SHIFT : modKeys) 
                       else modKeys
          delay = threadDelay millis
          millis = if delayMillis >= 0 then delayMillis else defaultKeyDelay

defaultKeyDelay = -1

getContext = toJsonFunction "get_context" (liftR $ return $ defaultContext) ()
    where defaultContext = object ["id" .= emptyStr, "title" .= emptyStr]
          emptyStr = "" :: String

writeText = toJsonFunction "write_text" (\t -> liftR (putStrLn t >> keyPress key_0 >> keyPress key_0))
            (Param "text" Nothing, ())

pause = toJsonFunction "pause" (\millis -> liftR $ threadDelay (1000 * millis))
        (Param "amount" Nothing, ())

instance FromJSON Direction where
    parseJSON "up" = return Up
    parseJSON "down" = return Down
    parseJSON "press" = return Press

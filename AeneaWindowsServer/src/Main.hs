{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main (main) where

import WindowsKeys
import JsonRpcServer
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
             (Param "direction" (Just "press"),
              (Param "count" (Just 1),
               (Param "delay" (Just (-1)), ())))))
              where keyPressF :: String -> [String] -> String -> Int -> Int -> IO ()
                    keyPressF key' modifiers direction count delay = withKeyPress key_ALT $ keyPress key_RIGHT

getContext = toJsonFunction "get_context" (liftR $ return $ defaultContext) ()
    where defaultContext = object ["id" .= emptyStr, "title" .= emptyStr]
          emptyStr = "" :: String

writeText = toJsonFunction "write_text" (\t -> liftR (putStrLn t >> keyPress key_0 >> keyPress key_0))
            (Param "text" Nothing, ())

pause = toJsonFunction "pause" (\millis -> liftR $ threadDelay (1000 * millis))
        (Param "amount" Nothing, ())

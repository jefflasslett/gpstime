{-# LANGUAGE OverloadedStrings #-}
module GPSDClient where

import Network.Socket
import System.IO
import System.Exit
import Data.Aeson
import qualified Data.Aeson.Types as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Options as O
import TPV;


catchTPV :: String -> String
catchTPV raw_json =
  let
    raw_json' = BL.pack raw_json
    json_value = decode raw_json' :: Maybe TPV
  in
    case json_value of
      Just val -> show json_value
      Nothing -> "No TPV"

catchTime :: String -> Maybe String
catchTime raw_json =
  let
    raw_json' = BL.pack raw_json
    json_value = decode raw_json' :: Maybe TPV
  in
    case json_value of
      Just val -> time val
      Nothing -> Nothing

findTime :: Handle -> IO String
findTime h =
  do
    raw_json <- hGetLine h
    case catchTime raw_json of
      Just s -> do
                 hClose h
                 return s
      Nothing -> findTime h


processGPSDStream  ::  O.Options -> IO String
processGPSDStream opts =
  do
    addr_infos <- getAddrInfo Nothing ( Just $ O.optHost opts ) ( Just $ O.optPort opts )

    let gpsd_server_address = head addr_infos

    sock <- socket ( addrFamily gpsd_server_address ) Stream defaultProtocol

    setSocketOption sock KeepAlive 1

    connect sock ( addrAddress gpsd_server_address )

    h <- socketToHandle sock ReadWriteMode

    hPutStrLn h "?WATCH={\"enable\":true,\"json\":true,\"scaled\":true}"

    findTime h


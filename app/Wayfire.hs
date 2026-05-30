{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Wayfire
  ( -- * Data types
    WayfireWindow(..)
  , WayfireEvent(..)
  , WayfireMessage(..)
  , Geometry(..)
  , WayfireOutput(..)
    -- * Socket I/O
  , readNextFrame
  , readOneMsg
    -- * Parsing
  , parseIncomingMessage
  , extractBindingId
  , parseOutputList
    -- * Command builders
  , listViews
  , listOutputs
  , watchEvents
  , registerBinding
  , focusView
  , setMinimized
  , configureView
  , moveViewToOutput
  ) where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Types (parseEither, Parser)
import Network.Socket (Socket)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Network.Socket.ByteString as SB
import Data.Bits ((.&.), shiftR, shiftL, (.|.))
import Data.Word (Word32)

-- ---------------------------------------------------------------------------
-- Data types
-- ---------------------------------------------------------------------------

data Geometry = Geometry
  { x      :: Int
  , y      :: Int
  , width  :: Int
  , height :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON Geometry
instance ToJSON   Geometry

data WayfireWindow = WayfireWindow
  { id         :: Int
  , pid        :: Int
  , title      :: String
  , outputId   :: Int
  , minimized  :: Bool
  , fullscreen :: Bool
  , appId      :: String
  , role       :: String
  , mapped     :: Bool
  , geometry   :: Geometry
  } deriving (Show, Generic)

instance FromJSON WayfireWindow where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \f -> case f of
        "appId"      -> "app-id"
        "outputId"   -> "output-id"
        "fullscreen" -> "fullscreen"
        _            -> f
    }

data WayfireOutput = WayfireOutput
  { id       :: Int
  , name     :: String
  , geometry :: Geometry
  , workarea :: Geometry
  } deriving (Show, Generic)

instance FromJSON WayfireOutput

data WayfireEvent = WayfireEvent
  { event :: String
  , view  :: WayfireWindow
  } deriving (Show, Generic)

instance FromJSON WayfireEvent

data WayfireMessage
  = Snapshot [WayfireWindow]    -- ^ Response to list-views
  | Signal WayfireEvent         -- ^ View lifecycle event from events/watch
  | CommandBinding Int          -- ^ A registered key binding fired; carries binding-id
  | OutputFocus Int             -- ^ An output gained keyboard focus; carries output-id
  | Heartbeat String            -- ^ Any other response (ok, errors, unhandled events)
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Internal framing
-- ---------------------------------------------------------------------------

bytesToWord32LE :: B.ByteString -> Word32
bytesToWord32LE bs =
  let g i = fromIntegral (B.index bs i) :: Word32
  in g 0 .|. (g 1 `shiftL` 8) .|. (g 2 `shiftL` 16) .|. (g 3 `shiftL` 24)

word32ToBytesLE :: Word32 -> B.ByteString
word32ToBytesLE w = B.pack
  [ fromIntegral  (w             .&. 0xFF)
  , fromIntegral ((w `shiftR` 8) .&. 0xFF)
  , fromIntegral ((w `shiftR` 16) .&. 0xFF)
  , fromIntegral ((w `shiftR` 24) .&. 0xFF)
  ]

encodePacket :: Value -> B.ByteString
encodePacket val =
  let body = BL.toStrict (encode val)
  in word32ToBytesLE (fromIntegral (B.length body)) <> body

wayfireCmd :: String -> Value -> B.ByteString
wayfireCmd method dat = encodePacket $ object ["method" .= method, "data" .= dat]

-- ---------------------------------------------------------------------------
-- Socket I/O
-- ---------------------------------------------------------------------------

-- | Read one complete framed message.  Returns Nothing when the socket closes.
-- Also returns any bytes that were read beyond the end of the message so the
-- caller can pass them on without losing data.
readNextFrame :: Socket -> B.ByteString -> IO (Maybe (B.ByteString, B.ByteString))
readNextFrame sock = loop
  where
    loop buf
      | B.length buf < 4 = do
          chunk <- SB.recv sock 4096
          if B.null chunk then return Nothing else loop (buf <> chunk)
      | otherwise =
          let payloadLen = fromIntegral (bytesToWord32LE (B.take 4 buf))
              needed     = 4 + payloadLen
          in if B.length buf < needed
               then do
                 chunk <- SB.recv sock 4096
                 if B.null chunk then return Nothing else loop (buf <> chunk)
               else return $ Just
                 ( B.take payloadLen (B.drop 4 buf)
                 , B.drop needed buf
                 )

-- | Like 'readNextFrame' but throws an IOError if the socket closes.
-- Use during sequential initialization where a closed socket is always fatal.
readOneMsg :: Socket -> B.ByteString -> IO (B.ByteString, B.ByteString)
readOneMsg sock buf = do
  result <- readNextFrame sock buf
  case result of
    Nothing -> ioError $ userError "Wayfire socket closed unexpectedly"
    Just r  -> return r

-- ---------------------------------------------------------------------------
-- Parsing
-- ---------------------------------------------------------------------------

-- | Parse a raw payload into a WayfireMessage.
parseIncomingMessage :: B.ByteString -> Either String WayfireMessage
parseIncomingMessage bytes =
  let lazy = BL.fromStrict bytes
  in case eitherDecode lazy :: Either String Value of
       Left err -> Left $ "Invalid JSON: " ++ err

       -- list-views returns a bare JSON array
       Right (Array _) ->
         case eitherDecode lazy :: Either String [WayfireWindow] of
           Right windows -> Right (Snapshot windows)
           Left err      -> Left $ "Failed to parse view list: " ++ err

       Right (Object obj) ->
         case parseEither (\o -> o .: "event" :: Parser String) obj of
           Right "command-binding" ->
             case parseEither (\o -> o .: "binding-id" :: Parser Int) obj of
               Right bid -> Right (CommandBinding bid)
               Left _    -> Right (Heartbeat "command-binding with no binding-id")
           Right "output-gain-focus" ->
             case parseEither (\o -> o .: "output" >>= withObject "output" (.: "id")) obj of
               Right oid -> Right (OutputFocus oid)
               Left _    -> Right (Heartbeat "output-gain-focus with no output.id")
           Right eventName ->
             case parseEither (\o -> o .: "view" :: Parser WayfireWindow) obj of
               Right wd -> Right (Signal (WayfireEvent eventName wd))
               Left _   -> Right (Heartbeat $ "Ignored event: " ++ eventName)
           Left _ ->
             Right (Heartbeat $ BC.unpack bytes)

       _ -> Left "Unexpected JSON value on wire."

-- | Pull the binding-id out of a command/register-binding response.
extractBindingId :: B.ByteString -> Maybe Int
extractBindingId bytes =
  case decode (BL.fromStrict bytes) of
    Just (Object obj) ->
      case parseEither (\o -> o .: "binding-id" :: Parser Int) obj of
        Right bid -> Just bid
        Left _    -> Nothing
    _ -> Nothing

-- ---------------------------------------------------------------------------
-- Command builders
-- ---------------------------------------------------------------------------

listViews :: B.ByteString
listViews = wayfireCmd "window-rules/list-views" (object [])

listOutputs :: B.ByteString
listOutputs = wayfireCmd "window-rules/list-outputs" (object [])

parseOutputList :: B.ByteString -> Either String [WayfireOutput]
parseOutputList = eitherDecode . BL.fromStrict

watchEvents :: B.ByteString
watchEvents = wayfireCmd "window-rules/events/watch" (object [])

-- | Register a key binding with no call-method so Wayfire sends a
-- "command-binding" event back to this socket when the key fires.
registerBinding :: String -> B.ByteString
registerBinding combo = wayfireCmd "command/register-binding" $ object
  [ "binding" .= combo ]

-- | Tell Wayfire to focus the given view ID.
focusView :: Int -> B.ByteString
focusView wid = wayfireCmd "window-rules/focus-view" $ object ["id" .= wid]

setMinimized :: Int -> Bool -> B.ByteString
setMinimized wid state = wayfireCmd "wm-actions/set-minimized" $ object
  [ "view_id" .= wid
  , "state"   .= state
  ]

-- | Move and resize a view to the given geometry.
configureView :: Int -> Geometry -> B.ByteString
configureView wid g = wayfireCmd "window-rules/configure-view" $ object
  [ "id"       .= wid
  , "geometry" .= g
  ]

-- | Move a view to a different output, keeping its geometry in the new output's
-- coordinate space. This changes the window's wset assignment, causing a
-- view-set-output event.
moveViewToOutput :: Int -> Geometry -> Int -> B.ByteString
moveViewToOutput wid g oid = wayfireCmd "window-rules/configure-view" $ object
  [ "id"        .= wid
  , "geometry"  .= g
  , "output_id" .= oid
  ]

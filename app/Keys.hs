{-# LANGUAGE OverloadedStrings #-}

module Keys where

-- import Network.Socket
-- import qualified Network.Socket.ByteString as SB

-- Helper to register standard XMonad core key combinations
-- setupXMonadKeys :: Socket -> IO ()
-- setupXMonadKeys sock = do
--     -- Super + Enter to spawn a terminal
--     SB.sendAll sock (encodeWayfirePacket $ registerKeybinding "<super> KEY_ENTER" "spawn-terminal")
--     -- Super + J to shift focus down the stack
--     SB.sendAll sock (encodeWayfirePacket $ registerKeybinding "<super> KEY_J" "focus-down")
--     -- Super + K to shift focus up the stack
--     SB.sendAll sock (encodeWayfirePacket $ registerKeybinding "<super> KEY_K" "focus-up")
--     -- Super + Shift + Q to close a window
--     SB.sendAll sock (encodeWayfirePacket $ registerKeybinding "<super> <shift> KEY_Q" "close-window")

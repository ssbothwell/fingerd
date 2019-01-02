{-# LANGUAGE OverloadedStrings #-}
module TelnetLib where

--import Control.Concurrent
import Control.Monad.State
import Data.Text()
import Data.Word
import Network.Socket hiding (recv)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket.ByteString (recv, sendAll)


data TelnetCommand =
      Message Word8     -- <128
    | SE               --  240
    | NOP              --  241
    | DataMark         --  242
    | Break            --  243
    | InterruptProcess --  244
    | AbortOutput      --  245
    | AreYouThere      --  246
    | EraseCharacter   --  247
    | EraseLine        --  248
    | GoAhead          --  249
    | SB               --  250
    | Will             --  251
    | Wont             --  252
    | Do               --  253
    | Dont             --  254
    | IAC              --  255
    deriving (Show, Eq)

data MessageMode = Normal | Command | SubNegotiation
data MessageState = MessageState { getBuffer :: ByteString, getMessageMode :: MessageMode }

toTelnetCommand :: Word8 -> TelnetCommand
toTelnetCommand w
    | w == 240  = SE
    | w == 241  = NOP
    | w == 242  = DataMark
    | w == 243  = Break
    | w == 244  = InterruptProcess
    | w == 245  = AbortOutput
    | w == 246  = AreYouThere
    | w == 247  = EraseCharacter
    | w == 248  = EraseLine
    | w == 249  = GoAhead
    | w == 250  = SB
    | w == 251  = Will
    | w == 252  = Wont
    | w == 253  = Do
    | w == 254  = Dont
    | w == 255  = IAC
    | otherwise = Message w


handleStream :: Word8 -> State MessageState ()
handleStream word = do
    buffer <- gets getBuffer
    mode <- gets getMessageMode
    case mode of
            Normal ->
                case toTelnetCommand word of
                    -- TODO: Handle '\r\n' in mid stream and '\x08' for clients
                    -- that stream characters as typed.
                    Message w -> put $ MessageState (BS.append buffer (BS.pack [w])) Normal
                    IAC -> put $ MessageState buffer Command
                    _ -> put $ MessageState buffer mode
            Command -> 
                -- TODO: Perform actual behavior for remaining TelnetCommands. 
                case toTelnetCommand word of
                    EraseCharacter -> 
                        case BS.unsnoc buffer of
                            Just (buffer', _) -> put $ MessageState buffer' Normal
                            Nothing -> put $ MessageState BS.empty Normal
                    EraseLine -> put $ MessageState BS.empty Normal
                    SB -> put $ MessageState buffer SubNegotiation
                    Will -> put $ MessageState buffer Command
                    Wont -> put $ MessageState buffer Command
                    Do -> put $ MessageState buffer Command
                    Dont -> put $ MessageState buffer Command
                    _ -> put $ MessageState buffer Normal
            SubNegotiation ->
                case toTelnetCommand word of
                    SE -> put $ MessageState buffer Normal
                    _ -> put $ MessageState buffer SubNegotiation

processStream :: ByteString -> MessageState
processStream bs = 
    let stream = BS.unpack bs
        startingState = MessageState BS.empty Normal
    in execState (mapM_ handleStream stream) startingState

prompt :: Socket -> ByteString -> IO ByteString
prompt sock prefix = do
    sendAll sock prefix
    rawMsg <- recv sock 1024
    let (MessageState msg _) = processStream rawMsg
    return msg

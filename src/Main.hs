{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Data.List (intersperse, find, intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.SQLite.Simple (Connection, open, query_)
import qualified Database.SQLite.Simple as SQLite
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import System.Exit (exitSuccess)
import Text.Trifecta (parseByteString)

import SqliteLib
import TelnetLib (prompt)
import Parser


-----------------------------
---- Types and Instances ----
-----------------------------

data Env = 
    Env { getConn        :: Connection
        , getControlSock :: Socket
        , getFingerSock  :: Socket
        , getStateTVar   :: TVar State 
        , getRChannel    :: TChan Msg
        } 

data ThreadEnv =
    ThreadEnv { getConn'        :: Connection
              , getControlSock' :: Socket
              , getStateTVar'   :: TVar State
              , getRChannel'    :: TChan Msg
              , getWChannel'    :: TChan Msg
              }

type Msg = String
type Username = String

data State = 
    State { getWhois :: [(Account, ThreadId)] } 
    deriving Show


--------------------------
---- Server Functions ----
--------------------------

-- I wish this worked:
--checkLogin' :: Connection -> Either Text Text -> Either Text Text -> IO (Either Text Account)
--checkLogin' conn acc pass = do
--    acc' <- acc
--    pass' <- pass
--    eAccount  <- selectAccount conn acc'
--    account <- eAccount
--    return $ checkPassword pass' account

checkPassword :: Text -> Account -> Either Text Account
checkPassword pass acc
    | pass /= getPassword acc = Left "Invalid Password"
    | otherwise = Right acc

checkLogin :: Connection -> Either Text Text -> Either Text Text -> IO (Either Text Account)
checkLogin _ (Left err') _ = print err' >> return (Left "Invalid Account")
checkLogin _ _ (Left err') = print err' >> return (Left "Invalid Password")
checkLogin conn (Right acc) (Right pass) = do
    eAccount <- selectAccount conn acc 
    return $ eAccount >>= checkPassword pass 

loginPrompt :: ReaderT ThreadEnv IO ()
loginPrompt = do
    stateTVar <- asks getStateTVar'
    conn <- asks getConn'
    sock <- asks getControlSock'

    thread <- liftIO $ myThreadId
    account <- liftIO $ prompt sock "Login: "
    password <- liftIO $ prompt sock "Password: "

    let parsedAccount = resultToEither $ parseByteString word mempty account
    let parsedPassword = resultToEither $ parseByteString word mempty password
    
    loginResult <- liftIO $ checkLogin conn parsedAccount parsedPassword
    case loginResult of
        Left err' -> liftIO (print err' >> sendMsg sock err') >> loginPrompt --conn sock state tchan readChan
        Right acc -> do
            state' <- liftIO $ readTVarIO stateTVar
            liftIO $ writeTVarIO stateTVar (State ((acc, thread):(getWhois state')))
            liftIO $ print $ T.append (getAccount acc) " Logged In"
            liftIO $ sendMsg sock "Login Succesful"
            handleControlQuery

addUser :: Connection -> User -> IO (Text)
addUser conn (User _ a b c d e) = do
    eInserted <- insertUser conn [a, b, c, d, e]
    case eInserted of
        Left err' -> print err' >> return "Problem adding user"
        Right res -> return $ formatUser res

getUser :: Connection -> Text -> IO (Text)
getUser conn username = do
    eUser <- selectUser conn (T.strip username)
    case eUser of
        Left err' -> print err' >> return "Problem finding user"
        Right user' -> return $ formatUser user'

getUsers :: Connection -> IO Text
getUsers conn = do
    rows <- query_ conn selectUsersQuery
    let usernames = map username rows
        newlineSeperated =
            T.concat $ (intersperse "\n" usernames) ++ pure (T.pack "\r\n")
    return newlineSeperated

-- TODO: Update to use new telnetd prompt function. See handleControlQuery below.
handleQuery :: Connection -> Socket -> IO ()
handleQuery conn sock = do
    msg <- recv sock 1024
    case msg of
        "\r\n" -> do
            users <- getUsers conn
            sendAll sock (encodeUtf8 users)
            close sock
        name -> do
            eUser <- getUser conn (decodeUtf8 name) 
            sendMsg sock eUser
            close sock
            return ()

----------------------
---- Control Loop ----
----------------------

processCommand :: Maybe (Account, ThreadId) -> ReaderT ThreadEnv IO ()
processCommand Nothing = loginPrompt
processCommand (Just (account, _)) = do
    stateTVar <- asks getStateTVar'
    conn <- asks getConn'
    sock <- asks getControlSock'
    wChannel <- asks getWChannel'
    state <- liftIO $ readTVarIO stateTVar

    liftIO $ do
        cmd <- prompt sock "> "
        cmdParse <- pure $ runParse cmd
        putStrLn $ show cmdParse
        case cmdParse of
            Right GetUsers -> getUsers conn >>= (sendMsg sock)
            Right (GetUser user) -> getUser conn user >>= (sendMsg sock)
            Right (AddUser user) -> addUser conn user >>= (sendMsg sock)
            Right (Echo msg) -> sendMsg sock msg
            Right Exit -> logout stateTVar account state >> sendMsg sock "Goodbye!" >> close sock
            Right Logout -> logout stateTVar account state
            Right Shutdown -> sendMsg sock "Shutting Down! Goodbye!" >> SQLite.close conn >> close sock >> exitSuccess
            Right Whois -> sendMsg sock (whois state)
            Right (Say msg) -> broadcast (T.unpack msg) wChannel
            Left err' -> sendMsg sock  "Command not recognized" >> (putStrLn $ show err')

handleControlQuery :: ReaderT ThreadEnv IO ()
handleControlQuery = do
    stateTVar <- asks getStateTVar'
    sock <- asks getControlSock'
    rChannelTVar <- asks getRChannel'

    state <- liftIO $ readTVarIO stateTVar
    thread <- liftIO $ myThreadId
    liftIO $ readTChanLoop sock rChannelTVar
    liftIO $ print state
    liftIO $ print thread
    let user = find (\(_, tid) -> tid == thread) $ getWhois state

    processCommand user 
    handleControlQuery 

forkReader :: ReaderT r IO a -> ReaderT r IO ThreadId
forkReader = undefined

logout :: TVar State -> Account -> State -> IO ()
logout stateTVar currAccount oldState = (writeTVarIO stateTVar) . State . filter f $ getWhois oldState
    where f (account', _) = currAccount /= account'

writeTVarIO :: TVar State -> State -> IO ()
writeTVarIO tvar = atomically . writeTVar tvar

broadcast :: String -> TChan Msg -> IO ()
broadcast msg wChannel = do
    liftIO . atomically $ writeTChan wChannel msg

sendMsg :: Socket -> Text -> IO ()
sendMsg sock msg = liftIO . sendAll sock . encodeUtf8 $ T.append msg "\r\n"

readTChanLoop :: Socket -> TChan Msg -> IO ()
readTChanLoop sock rChannel = liftIO . void . forkIO . forever $ do
      msg <- atomically $ readTChan rChannel
      sendMsg sock (T.pack msg)

whois :: State -> Text
whois curState = T.pack . intercalate ", " . fmap (show . fst) $ (getWhois curState)


--------------
---- Main ----
--------------

fingerd :: ReaderT Env IO ()
fingerd = forever $ do
    conn <- asks getConn
    sock <- asks getFingerSock
    (sock', _) <- lift $ accept sock
    lift $ do
        putStrLn "Got connection, handling query"
        handleQuery conn sock'
    
controld :: ReaderT Env IO ()
controld = forever $ do
    state <- asks getStateTVar
    conn <- asks getConn
    sock <- asks getControlSock
    rChannelTVar <- asks getRChannel
    (sock', _) <- lift $ accept sock

    void . lift . forkIO $ fix $ \loop -> do
        void . atomically $ readTChan rChannelTVar
        loop
    commLine <- lift . atomically $ cloneTChan rChannelTVar

    lift $ do
        putStrLn "Got connection, handling query"
        let threadEnv = ThreadEnv conn sock' state rChannelTVar commLine
        forkIO $ runReaderT handleControlQuery threadEnv

createSocket :: Integer -> IO Socket
createSocket port = do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                              Nothing (Just $ show port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock ReuseAddr 1 
    bind sock (addrAddress serveraddr)
    listen sock 1
    return sock

main :: IO ()
main = withSocketsDo $ do
    conn <- open "finger.db"
    controlSock <- createSocket 78
    fingerSock <- createSocket 79
    state <- atomically $ newTVar (State [])
    tchan <- newTChanIO
    let env = Env conn controlSock fingerSock state tchan

    _ <- forkIO $ runReaderT controld env 
    runReaderT fingerd env

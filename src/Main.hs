{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
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
        , getWChannel    :: TChan Msg
        } 

data ThreadEnv =
    ThreadEnv { getConn'        :: Connection
              , getControlSock' :: Socket
              , getStateTVar'   :: TVar State
              , getWChannel'    :: TChan Msg
              , getRChannel'    :: TChan Msg
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
        Left err' -> liftIO (print err') >> sendMsg err' >> loginPrompt
        Right acc -> do
            state' <- liftIO $ readTVarIO stateTVar
            writeTVarR $ State ((acc, thread):(getWhois state'))
            liftIO $ print $ T.append (getAccount acc) " Logged In"
            sendMsg "Login Succesful"
            handleControlQuery

addUser :: Connection -> User -> IO (Text)
addUser conn (User _ a b c d e) = do
    eInserted <- insertUser conn [a, b, c, d, e]
    case eInserted of
        Left err' -> print err' >> return "Problem adding user"
        Right res -> return $ formatUser res

getUser :: Connection -> Text -> IO Text
getUser conn username = do
    eUser <- selectUser conn (T.strip username)
    case eUser of
        Left err' -> print err' >> return "Problem finding user"
        Right user' -> return $ formatUser user'

getUsers :: Connection -> IO Text
getUsers conn = do
    rows <- query_ conn selectUsersQuery
    let usernames = map getUsername rows
        newlineSeperated =
            T.concat $ (intersperse "\n" usernames) ++ pure (T.pack "\r\n")
    return newlineSeperated

logout :: Account -> ReaderT ThreadEnv IO ()
logout currAccount = do
    stateTVar <- asks getStateTVar'
    state <- liftIO $ readTVarIO stateTVar
    writeTVarR $ State (filter f (getWhois state))
    where f (account', _) = currAccount /= account'

writeTVarR :: State -> ReaderT ThreadEnv IO ()
writeTVarR state = do
    stateTVar <- asks getStateTVar'
    liftIO . atomically $ writeTVar stateTVar state

broadcast :: String -> ReaderT ThreadEnv IO ()
broadcast msg = do
    wChannel <- asks getWChannel'
    liftIO . atomically $ writeTChan wChannel msg

sendMsg :: Text -> ReaderT ThreadEnv IO ()
sendMsg msg = do
    sock <- asks getControlSock'
    liftIO . sendAll sock . encodeUtf8 $ T.append msg "\r\n"

forkReader :: ReaderT r IO () -> ReaderT r IO ThreadId
forkReader action = do
    env <- ask
    liftIO . forkIO $ runReaderT action env

readTChanLoop :: ReaderT ThreadEnv IO ()
readTChanLoop = void . forkReader . forever $ do
    rChannel <- asks getRChannel'
    msg <- liftIO . atomically $ readTChan rChannel
    sendMsg (T.pack msg)

whois :: State -> Text
whois curState = T.pack . intercalate ", " . fmap (show . fst) $ (getWhois curState)



----------------------
---- Control Loop ----
----------------------

processCommand :: Maybe (Account, ThreadId) -> ReaderT ThreadEnv IO ()
processCommand Nothing = loginPrompt
processCommand (Just (account, _)) = do
    stateTVar <- asks getStateTVar'
    conn <- asks getConn'
    sock <- asks getControlSock'
    state <- liftIO $ readTVarIO stateTVar

    cmd <- liftIO $ prompt sock "> "
    cmdParse <- pure $ runParse cmd
    liftIO $ putStrLn $ show cmdParse
    case cmdParse of
        Right GetUsers -> liftIO (getUsers conn) >>= sendMsg
        Right (GetUser user) -> liftIO (getUser conn user) >>= sendMsg
        Right (AddUser user) -> liftIO (addUser conn user) >>= sendMsg
        Right (Echo msg) -> sendMsg msg
        Right Exit -> logout account >> sendMsg "Goodbye!" >> liftIO (close sock)
        Right Logout -> logout account
        Right Shutdown -> sendMsg "Shutting Down! Goodbye!" >> liftIO (SQLite.close conn >> close sock >> exitSuccess)
        Right Whois -> sendMsg (whois state)
        Right (Say msg) -> broadcast (T.unpack msg)
        Left err' -> sendMsg "Command not recognized" >> liftIO (putStrLn $ show err')

handleControlQuery :: ReaderT ThreadEnv IO ()
handleControlQuery = do
    stateTVar <- asks getStateTVar'

    state <- liftIO $ readTVarIO stateTVar
    thread <- liftIO $ myThreadId
    readTChanLoop
    liftIO $ print state
    liftIO $ print thread
    let user = find (\(_, tid) -> tid == thread) $ getWhois state

    processCommand user 
    handleControlQuery 

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
            sendAll sock (encodeUtf8 eUser)
            close sock
            return ()


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
    stateTVar <- asks getStateTVar
    conn <- asks getConn
    sock <- asks getControlSock
    wChannel<- asks getWChannel
    rChannel <- liftIO . atomically $ cloneTChan wChannel
    (sock', _) <- lift $ accept sock


    lift $ do
        putStrLn "Got connection, handling query"
        let threadEnv = ThreadEnv conn sock' stateTVar wChannel rChannel
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
    wChannel <- newTChanIO
    let env = Env conn controlSock fingerSock state wChannel

    _ <- forkIO $ runReaderT controld env 
    runReaderT fingerd env

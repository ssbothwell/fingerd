{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative
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
import Data.ByteString (ByteString)
--import qualified Data.ByteString as BS
import Network.Socket.ByteString (recv, sendAll)
import System.Exit (exitSuccess)
import Text.Trifecta

import SqliteLib
import TelnetLib (prompt)


-----------------------------
---- Types and Instances ----
-----------------------------

data Env = 
    Env { dbConn      :: Connection
        , controlSock :: Socket
        , fingerSock  :: Socket
        , getState    :: TVar State 
        , rChannel    :: TChan Msg
        } 

data ThreadEnv =
    ThreadEnv { dbConn'      :: Connection
              , controlSock' :: Socket
              , stateTVar'   :: TVar State
              , rChannel'    :: TChan Msg
              , wChannel'    :: TChan Msg
              }

type Msg = (String)
type Username = String

data State = 
    State { getWhois :: [(Account, ThreadId)] } 
    deriving Show

data Command = 
      GetUsers
    | GetUser Text
    | AddUser User
    | Echo Text
    | Exit
    | Shutdown
    | Logout
    | Whois
    | Say Text
    deriving (Eq, Show)


------------------------
---- Command Parser ----
------------------------

word :: Parser Text
word = token $ do
    str <- some letter
    return $ T.pack str

parserGetUsers :: Parser Command
parserGetUsers = symbol "getUsers" *> return GetUsers

parserGetUser :: Parser Command
parserGetUser = token $ do
    _ <- string "getUser"
    _ <- char ' '
    username <- word
    return $ GetUser username

parserAddUser :: Parser Command
parserAddUser = token $ do
    _ <- string "addUser"
    _ <- char ' '
    username' <- word
    shell' <- word
    homeDir' <- word
    realName' <- word
    phone' <- word
    return $ AddUser (User 0 username' shell' homeDir' realName' phone')

parserExit :: Parser Command
parserExit = token $ do
    _ <- symbol "exit"
    return $ Exit

parserShutdown :: Parser Command
parserShutdown = token $ do
    _ <- symbol "shutdown"
    return $ Shutdown

parserLogout :: Parser Command
parserLogout = token $ do
    _ <- symbol "logout"
    return $ Logout

parserWhois :: Parser Command
parserWhois = token $ do
    _ <- symbol "whois"
    return $ Whois

parserSay :: Parser Command
parserSay = token $ do
    _ <- symbol "say"
    str <- anyChar `manyTill` (char '\r' <|> char '\n')
    return $ Say (T.pack str)

parserEcho :: Parser Command
parserEcho = token $ do
    _ <- symbol "echo"
    str <- anyChar `manyTill` (char '\r' <|> char '\n')
    return $ Echo (T.pack str)

commandParser :: Parser Command
commandParser =  parserGetUsers 
             <|> parserGetUser 
             <|> parserAddUser 
             <|> parserExit
             <|> parserShutdown
             <|> parserEcho
             <|> parserLogout
             <|> parserWhois
             <|> parserSay

runParse :: ByteString -> Either Text Command
runParse = resultToEither . parseByteString commandParser mempty
    
resultToEither :: Result a -> Either Text a
resultToEither (Failure err') = Left . T.pack $ show err'
resultToEither (Success a) = Right a


--------------------------
---- Server Functions ----
--------------------------

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
    stateTVar <- asks stateTVar'
    conn <- asks dbConn'
    sock <- asks controlSock'

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
    stateTVar <- asks stateTVar'
    conn <- asks dbConn'
    sock <- asks controlSock'
    wChannel <- asks wChannel'
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
    stateTVar <- asks stateTVar'
    sock <- asks controlSock'
    rChannelTVar <- asks rChannel'

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
    conn <- asks dbConn
    sock <- asks fingerSock
    (sock', _) <- lift $ accept sock
    lift $ do
        putStrLn "Got connection, handling query"
        handleQuery conn sock'
    
controld :: ReaderT Env IO ()
controld = forever $ do
    state <- asks getState
    conn <- asks dbConn
    sock <- asks controlSock
    rChannelTVar <- asks rChannel
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

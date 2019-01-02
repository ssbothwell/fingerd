{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever, when)
import Control.Monad.Fix
import Control.Monad.Trans.Reader
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
    Env { dbConn :: Connection
        , controlSock :: Socket
        , fingerSock :: Socket
        , getState :: TVar State 
        , getTChan :: TChan Msg
        , getMsgCount :: Int
        } 

type Msg = (Int, String)
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
    
loginPrompt :: Connection -> Socket -> TVar State -> TChan Msg -> TChan Msg -> Int -> IO ()
loginPrompt conn sock state tchan readChan msgInt = do
    thread <- myThreadId
    account <- prompt sock "Login: "
    password <- prompt sock "Password: "

    let parsedAccount = resultToEither $ parseByteString word mempty account
    let parsedPassword = resultToEither $ parseByteString word mempty password
    
    loginResult <- checkLogin conn parsedAccount parsedPassword
    case loginResult of
        Left err' -> print err' >> sendMsg err' >> loginPrompt conn sock state tchan readChan msgInt
        Right acc -> do
            state' <- readTVarIO state
            writeTVar' $ State ((acc, thread):(getWhois state'))
            print $ T.append (getAccount acc) " Logged In"
            sendMsg "Login Succesful"
            handleControlQuery conn sock state tchan readChan msgInt
    where sendMsg msg = sendAll sock . encodeUtf8 $ T.append msg "\r\n"
          writeTVar' = atomically . writeTVar state

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
            sendMsg eUser
            close sock
            return ()
    where sendMsg msg = sendAll sock . encodeUtf8 $ T.append msg "\r\n"

handleControlQuery :: Connection -> Socket -> TVar State -> TChan Msg -> TChan Msg -> Int -> IO ()
handleControlQuery conn sock state writeChan readChan msgInt = do
    state' <- readTVarIO state
    thread <- myThreadId
    print state'
    print thread

    _ <- readTChanLoop

    let userStatus = find (\(_, tid) -> tid == thread) $ getWhois state'
    case userStatus of
        Nothing -> loginPrompt conn sock state writeChan readChan msgInt
        Just (account, _) -> do
            cmd <- prompt sock "> "
            cmdParse <- pure $ runParse cmd
            putStrLn $ show cmdParse
            case cmdParse of
                Right GetUsers -> getUsers conn >>= sendMsg >> loop msgInt
                Right (GetUser user) -> getUser conn user >>= sendMsg >> loop msgInt
                Right (AddUser user) -> addUser conn user >>= sendMsg >> loop msgInt
                Right (Echo msg) -> sendMsg msg >> loop msgInt
                Right Exit -> logout account state' >> sendMsg "Goodbye!" >> close sock
                Right Logout -> logout account state' >> loop msgInt
                Right Shutdown -> sendMsg "Shutting Down! Goodbye!" >> SQLite.close conn >> close sock >> exitSuccess
                Right Whois -> sendMsg (whois state') >> loop msgInt
                Right (Say msg) -> broadcast (T.unpack msg) >> loop (msgInt+1)
                Left err' -> sendMsg  "Command not recognized" >> (putStrLn $ show err') >> loop msgInt
    where loop msgInt = handleControlQuery conn sock state writeChan readChan msgInt
          sendMsg msg = sendAll sock . encodeUtf8 $ T.append msg "\r\n"
          writeTVar' = atomically . writeTVar state
          logout currAccount oldState = writeTVar' . State $ filter (\(account', _) -> currAccount /= account') $ getWhois oldState
          whois curState = T.pack . intercalate ", " . fmap (show . fst) $ (getWhois curState)
          broadcast msg = atomically $ writeTChan writeChan (msgInt, msg)
          readTChanLoop = forkIO . forever $ do
                (nextNum, msg) <- atomically $ readTChan readChan
                when (msgInt /= nextNum) $ sendMsg $ T.pack msg


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
    tchan <- asks getTChan
    msgInt <- asks getMsgCount
    (sock', _) <- lift $ accept sock

    _ <- lift . forkIO $ fix $ \loop -> do
        (_, _) <- atomically $ readTChan tchan
        loop
    commLine <- lift . atomically $ cloneTChan tchan 

    lift $ do
        putStrLn "Got connection, handling query"
        forkIO $ handleControlQuery conn sock' state tchan commLine msgInt

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
    let env = Env conn controlSock fingerSock state tchan 0

    _ <- forkIO $ runReaderT controld env 
    runReaderT fingerd env

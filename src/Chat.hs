{-# LANGUAGE ScopedTypeVariables #-}
-- | CS240h Lab 2 Chat Server
module Chat (chat) where

import Network
import Control.Concurrent (forkFinally)
import Data.IORef
import System.IO
import System.Environment (getEnv)
import Control.Monad (forever)

-- Note that we *dont* need any locks on our handle since GHC gives
-- them to us already
-- http://hackage.haskell.org/package/base-4.3.1.0/docs/src/GHC-IO-Handle-Types.html
data Client = Client {idx :: Int, hClient :: Handle} deriving Show

-- atomicModifyIORef saves us from a lock here
addNewClient :: Client -> IORef [Client] -> IO ()
addNewClient client clientsRef = doChange >> return ()
  where doChange = atomicModifyIORef clientsRef mkClients
        mkClients clients = (clients, client:clients)


prefixMessage :: Client -> String -> String
prefixMessage (Client idxNum _) = (((show idxNum) ++ ": ") ++)

otherClients :: Client -> IORef [Client] -> IO [Client]
otherClients (Client idxNum _) clients =
  readIORef clients >>= return . filter ((idxNum /=) . idx)

tellClient :: String -> Client -> IO ()
tellClient = flip $ hPutStrLn . hClient

-- We split this out for the sake of unit testing
talkAction :: (String -> String) -> (IORef [Client] -> IO [Client])
              -> IORef [Client] ->  Handle -> IO ()
talkAction prefixMe everyoneElse currentClients hndl = do
  toSay <- hGetLine hndl >>= return . prefixMe
  putStrLn $ "About to say: " ++ toSay
  everyoneElse currentClients >>= mapM_ (tellClient toSay)

talk :: Client -> IORef [Client] -> IO ()
talk client@(Client _ hndl) currentClients = doTalk
  where prefixMe = prefixMessage client
        everyoneElse = otherClients client
        doTalk = forever $ talkAction prefixMe everyoneElse currentClients hndl

chatLoop :: Socket -> IO ()
chatLoop socket = loop [1..]
  where
    currentClients' :: IO (IORef [Client])
    currentClients' = newIORef ([] :: [Client])
    loop [] = return ()         -- Suppress warning
    loop (clientIdx:idxs) = do
      putStrLn $ "About to connect to client" ++ (show clientIdx)
      (handle, hostName, _) <- accept socket
      putStrLn $ "Got a client from host " ++ hostName
      currentClients <- currentClients'
      hSetBuffering handle LineBuffering
      let newClient = Client clientIdx handle
      addNewClient newClient currentClients
      newThIdx <-
        forkFinally (talk newClient currentClients) (\_ -> hClose handle)
      putStrLn $ "Forked thread " ++ show newThIdx
      loop idxs

chatOnPort :: Int -> IO ()
chatOnPort port = withSocketsDo $ do
  sock <- listenOn $ PortNumber $ fromIntegral port
  chatLoop sock

getPort :: IO Int
getPort = getEnv "CHAT_SERVER_PORT" >>= return . read

chat :: IO ()
chat = getPort >>= chatOnPort

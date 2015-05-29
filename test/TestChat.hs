{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Test.Hspec

import System.IO.Temp
import System.IO (hSeek, hIsOpen, hIsWritable, SeekMode(AbsoluteSeek))
import System.IO.Error
import Data.IORef
import System.Directory
import System.Posix.Env
import Control.Concurrent
import Control.Exception (bracket)

import Chat

tempHandleDir :: String
tempHandleDir = "test/generated"

tempHandlePattern :: String
tempHandlePattern = "clientFile.tmp"

-- | Run a test with 1 temporary client.
withTempClient :: Int -> (Client -> IO a) -> IO a
withTempClient cIdx f = withTempFile tempHandleDir tempHandlePattern axn
  where axn _ handle = f $ Client cIdx handle

-- | Run a test with 0 or more temporary clients.
withTempClients :: [Int] -> ([Client] -> IO a) -> IO a
withTempClients idxs f = bracket beforeTest afterTest test
  where beforeTest  = mapM openFile idxs >>= return . unzip
        openFile = (\_ -> openTempFile tempHandleDir tempHandlePattern)
        afterTest   = (\(paths, _) -> mapM_ removeFile paths)
        test  = (\(_, handles) -> f $ zipWith Client idxs handles)

-- | Send a message to a client and rewind his mocked file handle, which we need to do
-- in order to read back what we wrote.
tellClient' :: String -> Client -> IO ()
tellClient' s c@(Client _ h) = tellClient s c >> hSeek h AbsoluteSeek 0

-- | Rewind the file handles
rewind :: [Client] -> IO ()
rewind cs = mapM_ (\(Client _ h) -> hSeek h AbsoluteSeek 0) cs

-- | Run the tests.
main :: IO ()
main = hspec $ describe "Testing Lab 2" $ do
  let expectS s c = askClient c `shouldReturn` s
  let withThree = withTempClients [1..3]
  let withOne = withTempClient 1

  describe "prefix message" $ do
    it "prefixes 1" $ withTempClient 1 $ \c -> do
      prefixMessage c "hello" `shouldBe` "1: hello"
    it "prefixes 2" $ withTempClient 2 $ \c -> do
      prefixMessage c "hello" `shouldBe` "2: hello"

  describe "other clients" $ do
    it "ignores itself" $ withThree $ \cs@[c1, c2, c3] -> do
      cRef <- newIORef cs
      otherClients c1 cRef `shouldReturn` [c2, c3]
      otherClients c2 cRef `shouldReturn` [c1, c3]
      otherClients c3 cRef `shouldReturn` [c1, c2]

  describe "client CRUD" $ do
    it "adds a client" $ withOne $ \c -> do
      cRef <- newIORef ([] :: [Client])
      readIORef cRef `shouldReturn` []
      addClient c cRef
      readIORef cRef `shouldReturn` [c]
      addClient c cRef
      readIORef cRef `shouldReturn` [c, c]
    it "removes a client" $ withTempClients [1, 2] $ \[c1, c2] -> do
      cRef <- newIORef ([] :: [Client])
      readIORef cRef `shouldReturn` []
      addClient c1 cRef
      readIORef cRef `shouldReturn` [c1]
      addClient c2 cRef
      readIORef cRef `shouldReturn` [c2, c1]
      removeClient c1 cRef
      readIORef cRef `shouldReturn` [c2]
      removeClient c2 cRef
      readIORef cRef `shouldReturn` []

  describe "client greetings" $ do
    it "says bye" $ withThree $ \cs -> do
      cRef <- newIORef cs
      readIORef cRef >>= mapM_ (\me -> sayBye me cRef)
      readIORef cRef >>= rewind
      readIORef cRef >>= mapM_ (expectS "1 has left")
      readIORef cRef >>= mapM_ (expectS "2 has left")
      readIORef cRef >>= mapM_ (expectS "3 has left")
    it "says hi" $ withThree $ \cs -> do
      cRef <- newIORef cs
      readIORef cRef >>= mapM_ (\me -> sayHi me cRef)
      readIORef cRef >>= rewind
      readIORef cRef >>= mapM_ (expectS "1 has joined")
      readIORef cRef >>= mapM_ (expectS "2 has joined")
      readIORef cRef >>= mapM_ (expectS "3 has joined")

  describe "client IO" $ do
    it "can't ask an empty client" $ withOne $ \c-> do
      askClient c `shouldThrow` isEOFError
    it "opens a handle corectly" $ withOne $ \(Client _ h) -> do
      hIsOpen h `shouldReturn` True
      hIsWritable h `shouldReturn` True
    it "can ask and tell" $ withOne $ \c -> do
      tellClient' "hello" c
      askClient c `shouldReturn` "hello"

  describe "find the port" $ do
    it "can read from the environment" $ do
      setEnv "CHAT_SERVER_PORT" "5050" True
      getPort `shouldReturn` 5050

  describe "talking between clients" $ do
    it "should talk to others" $ withThree $ \cs@[c1, c2, c3] -> do
      cRef <- newIORef cs
      tellClient' "hello world" c1
      talkAction' tellClient' (prefixMessage c1) (otherClients c1) cRef c1
      askClient c1 `shouldThrow` isEOFError
      mapM_ (\c -> askClient c `shouldReturn` "1: hello world") [c2, c3]

    it "should work in other threads" $ withThree $ \cs@[c1, c2, c3] -> do
      cRef <- newIORef cs
      tellClient' "hello world" c1
      _ <- forkIO $ talk c1 cRef
      threadDelay 1000
      mapM_ (\(Client _ h) -> hSeek h AbsoluteSeek 0) cs
      mapM_ (\c -> askClient c `shouldReturn` "1: hello world") [c2, c3]

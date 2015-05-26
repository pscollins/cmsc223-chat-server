{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Test.Hspec

import System.IO.Temp
import System.IO
import System.IO.Error
import Data.IORef
import System.Directory
import System.Posix.Env

import Chat

tempHandleDir :: String
tempHandleDir = "test/generated"

tempHandlePattern :: String
tempHandlePattern = "clientFile.tmp"

withTempClient :: Int -> (Client -> IO a) -> IO a
withTempClient cIdx f = withTempFile tempHandleDir tempHandlePattern axn
  where axn _ handle = f $ Client cIdx handle

-- FIXME: use Exception.bracket
withTempClients :: [Int] -> ([Client] -> IO a) -> IO a
withTempClients idxs f = do
  (paths, handles) <-
    mapM (\_ -> openTempFile tempHandleDir tempHandlePattern) idxs >>= return . unzip
  res <- f $ zipWith Client idxs handles
  mapM_ removeFile paths
  return res

tellClient' :: String -> Client -> IO ()
tellClient' s c@(Client _ h) = tellClient s c >> hSeek h AbsoluteSeek 0

main :: IO ()
main = hspec $ describe "Testing Lab 2" $ do
  describe "prefix message" $ do
    it "prefixes 1" $ withTempClient 1 $ \c -> do
      prefixMessage c "hello" `shouldBe` "1: hello"
    it "prefixes 2" $ withTempClient 2 $ \c -> do
      prefixMessage c "hello" `shouldBe` "2: hello"

  describe "other clients" $ do
    it "ignores itself" $ withTempClients [1..3] $ \cs@[c1, c2, c3] -> do
      cRef <- newIORef cs
      otherClients c1 cRef `shouldReturn` [c2, c3]
      otherClients c2 cRef `shouldReturn` [c1, c3]
      otherClients c3 cRef `shouldReturn` [c1, c2]

  describe "add client" $ do
    it "adds a client" $ withTempClient 1 $ \c -> do
      cRef <- newIORef ([] :: [Client])
      readIORef cRef `shouldReturn` []
      addClient c cRef
      readIORef cRef `shouldReturn` [c]

  describe "client IO" $ do
    it "can't ask an empty client" $ withTempClient 1 $ \c-> do
      askClient c `shouldThrow` isEOFError
    it "opens a handle corectly" $ withTempClient 1 $ \(Client _ h) -> do
      hIsOpen h `shouldReturn` True
      hIsWritable h `shouldReturn` True
    it "can ask and tell" $ withTempClient 1 $ \c -> do
      tellClient' "hello" c
      askClient c `shouldReturn` "hello"

  describe "find the port" $ do
    it "can read from the environment" $ do
      setEnv "CHAT_SERVER_PORT" "5050" True
      getPort `shouldReturn` 5050

  describe "talking between clients" $
    -- it "should not have talked before talking" $ do
    --   mapM_ (\c -> askClient c `shouldThrow` isEOFError) cs
    it "should talk to others" $ withTempClients [1..3] $ \cs@[c1, c2, c3] -> do
      cRef <- newIORef cs
      tellClient' "hello world" c1
      talkAction' tellClient' (prefixMessage c1) (otherClients c1) cRef c1
      askClient c1 `shouldThrow` isEOFError
      mapM_ (\c -> askClient c `shouldReturn` "1: hello world") [c2, c3]

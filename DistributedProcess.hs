{-# LANGUAGE DeriveDataTypeable,ScopedTypeVariables #-}
module PingPong where
import Control.Concurrent ( threadDelay )
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport ( closeTransport )
import Network.Transport.TCP

server :: DynamicT -> Process ()
server st = do
    (cid,x) :: (ProcessId,Int) <- expect
    liftIO $ putStrLn $ "Got  a Ping with value : " ++ (show x)
    case x of
      4 -> do
        liftIO $ putStrLn $ "UPGRADE"
        liftIO $ st
      _ -> do
        send cid x
        liftIO $ putStrLn $ "Sent a Pong with value : " ++ (show x)
        server st

client :: DynamicT -> Int -> ProcessId -> Process ()
client st 10 sid = do
  liftIO $ putStrLn "DONE"
client st c sid = do
  me <- getSelfPid
  send sid (me,c)
  liftIO $ putStrLn $ "Sent a Ping with value : " ++ (show c)
  (v :: Int) <- expect
  liftIO $ putStrLn $ "Got  a Pong with value : " ++ (show v)
  client st (c+1) sid

ignition :: DynamicT -> Process ()
ignition st= do
    -- start the server
    sid <- spawnLocal $ server st
    -- start the client
    cid <- spawnLocal $ client st 0 sid
    return ()
    liftIO $ threadDelay 100000-- wait a while

type DynamicT = IO ()

main :: DynamicT -> IO ()
main st = do
    Right transport <- createTransport "127.0.0.1" "8080"
                            defaultTCPParameters
    node <- newLocalNode transport initRemoteTable
    runProcess node $ ignition st
    closeTransport transport
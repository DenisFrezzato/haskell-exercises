import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception

threadDelayMS :: Int -> IO ()
threadDelayMS ms = threadDelay $ ms * 1000

handleThreadEnd :: Either SomeException a -> IO ()
handleThreadEnd (Left e) = print e
handleThreadEnd _ = pure ()

forkIO' :: IO () -> IO (ThreadId, MVar ())
forkIO' io = do
    mvar <- newEmptyMVar
    threadId <- forkFinally io (\e -> handleThreadEnd e >> putMVar mvar ())
    return (threadId, mvar)

main = do
    (threadId1, mvar1) <- forkIO' $ threadDelayMS 2500 >> putStrLn "Forked IO #1 has done."
    (_, mvar2) <- forkIO' $ threadDelayMS 1500 >> putStrLn "Forked IO #2 has done."
    (_, mvar3) <- forkIO' $ threadDelayMS 1000 >> putStrLn "Forked IO #3 has done."
    throwTo threadId1 $ ErrorCall "Forked IO #1 has been halted."
    sequence_ [takeMVar mvar1, takeMVar mvar2, takeMVar mvar3]

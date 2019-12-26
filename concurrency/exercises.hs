import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import System.Timeout

threadDelayMS :: Int -> IO ()
threadDelayMS ms = threadDelay $ ms * 1000

handleThreadEnd :: Either SomeException a -> IO ()
handleThreadEnd (Left e) = print e
handleThreadEnd _ = pure ()

forkIO' :: IO () -> IO (MVar ())
forkIO' io = do
    mvar <- newEmptyMVar
    threadId <- forkFinally io (\e -> handleThreadEnd e >> putMVar mvar ())
    return mvar

main = do
    mvar1 <- forkIO' $ do
        m <- timeout 2000000 $ threadDelayMS 2500
        case m of
          Just _ -> putStrLn "Forked IO #1 has done."
          Nothing -> putStrLn "Forked IO #1 has timed out."
    mvar2 <- forkIO' $ threadDelayMS 1500 >> putStrLn "Forked IO #2 has done."
    mvar3 <- forkIO' $ threadDelayMS 1000 >> putStrLn "Forked IO #3 has done."
    putStrLn "Waiting..."
    sequence_ [takeMVar mvar1, takeMVar mvar2, takeMVar mvar3]

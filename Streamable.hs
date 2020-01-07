{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (Bool, Int, Show, String, ($), (<$>), (.))
import Data.Either
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.IO
import Control.Monad
import Control.Exception

class (Monoid a, Show a) => Streamable a where
    readChunk :: Handle -> Int -> IO a
    null :: a -> Bool

instance Streamable String where
    readChunk h size = T.unpack . TE.decodeUtf8 <$> B.hGetSome h size
    null = null

instance Streamable T.Text where
    readChunk h size = TE.decodeUtf8 <$> B.hGetSome h size  
    null = T.null

instance Streamable TL.Text where
    readChunk h size = TLE.decodeUtf8 <$> BL.hGet h size
    null = TL.null

instance Streamable B.ByteString where
    readChunk = B.hGetSome
    null = B.null

instance Streamable BL.ByteString where
    readChunk = BL.hGet
    null = BL.null

data StreamException = EOFException | OtherException SomeException deriving (Show)

instance Exception StreamException

{- streamFile path bytes
Given a file located at `path` and a chunk size `bytes`, `makeFileStream` will:
return an IO action, which, when executed, will return `Either StreamException a`
If successful, it will have read `bytes` from the given file and returned some
`Streamable a` data, otherwise it will return StreamException.
Make sure that your `StreamException` can encapsulate the case when the file has
been fully read (reached EOF), which will signal to the caller that they should stop.
`makeFileStream` should never throw an exception: all exceptions should be
abstracted into the `StreamException` type.
-}
makeFileStream :: Streamable a => FilePath -> Int -> IO (Either StreamException a)
makeFileStream filepath chunkSize = withBinaryFile filepath ReadMode (\h -> do
   eRes <- try $ readChunk h chunkSize
   case eRes of
     Left e -> return $ Left $ OtherException e
     Right a -> return $ Right a)

main = do
    eRes <- makeFileStream "./io/source.txt" 2048
    case eRes of
      Left e -> print e
      Right a -> putStrLn a
    return ()

{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow ((>>>))
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE

main = B.readFile "source.txt" >>= (
    TE.decodeUtf8
    >>> TE.encodeUtf16BE
    >>> B.writeFile "dest.txt")

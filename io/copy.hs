{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B

main = B.readFile "source.txt" >>= B.writeFile "dest.txt"


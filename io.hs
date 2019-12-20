{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B

main = do
    let space = 32
    let underscore = 95
    fileContent <- B.readFile "io.txt" 
    B.writeFile "io-up.txt" $ B.map (\w -> if w == space then underscore else w) fileContent
    return ()

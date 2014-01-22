{-# LANGUAGE TemplateHaskell #-}
module Graphics.Rendering.Shader.TH where


import           Language.Haskell.TH
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

-- | Taken from Michael Snoyberg
-- http://hackage.haskell.org/package/file-embed-0.0.6/docs/src/Data-FileEmbed.html#embedFile
embedFile :: FilePath -> Q Exp
embedFile fp = (runIO $ B.readFile fp) >>= bsToExp

bsToExp :: B.ByteString -> Q Exp
bsToExp bs = do
    helper <- [| stringToBs |]
    let chars = B8.unpack bs
    return $! AppE helper $! LitE $! StringL chars

stringToBs :: String -> B.ByteString
stringToBs = B8.pack


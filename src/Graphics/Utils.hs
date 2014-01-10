module Graphics.Utils where

import           Graphics.Rendering.OpenGL
import           Control.Monad
import           Data.Char
import           System.IO (hPutStrLn, stderr)
import           System.Exit (exitFailure)
import qualified Data.ByteString as B


printError :: IO ()
printError = get errors >>= mapM_ (hPutStrLn stderr . ("GL: "++) . show)


makeShader :: ShaderType -> B.ByteString -> IO Shader
makeShader ty src = do
    s <- createShader ty
    shaderSourceBS s $= src
    compileShader s
    s'Ok <- get $ compileStatus s
    unless s'Ok $ do
        slog <- get $ shaderInfoLog s
        putStrLn $ "Log:" ++ slog
        exitFailure
    printError
    return s


makeProgram :: [Shader] -> [(String, AttribLocation)] -> IO Program
makeProgram shaders attributes = do
    p <- createProgram
    mapM_ (attachShader p) shaders
    mapM_ (\(name, loc) -> attribLocation p name $= loc) attributes
    linkProgram p
    p'Ok <- get $ linkStatus p
    validateProgram p
    status <- get $ validateStatus p
    unless (p'Ok && status) $ do
        plog <- get $ programInfoLog p
        putStrLn plog
        printError
        exitFailure
    return p


bindVBO :: BufferObject -> VertexArrayDescriptor a -> AttribLocation -> IO ()
bindVBO vbo dsc loc = do
    bindBuffer ArrayBuffer $= Just vbo
    vertexAttribPointer loc $= (ToFloat, dsc)
    vertexAttribArray loc $= Enabled


charIndex :: Char -> Int
charIndex = (\a -> a-33) . fromIntegral . ord


charToIndices :: Char -> [Int]
charToIndices ' '  = []
charToIndices '\n' = []
charToIndices c = i's
    where i's = [ tl, tr, br
                , tl, br, bl
                ]
          ndx = charIndex c
          n_32 :: Double
          n_32= fromIntegral ndx/32.00
          n__ :: Integer
          n__ = floor n_32
          n'  = ndx + fromIntegral n__
          tl  = n'
          tr  = tl+1
          bl  = tl+33
          br  = tl+34


fontUVPairs :: [[GLfloat]]
fontUVPairs = [ map (/32) [x,y] | y <- [0..6], x <- [0..32] ]


charToUVs :: Char -> [GLfloat]
charToUVs ' '  = []
charToUVs '\n' = []
charToUVs c    = concatMap (\n -> fontUVPairs !! fromIntegral n) (charToIndices c)


charToVerts :: Char -> [GLfloat]
charToVerts _ = [ 0, 0
                , 1, 0
                , 1, 1

                , 0, 0
                , 1, 1
                , 0, 1
                ]


stringToUVs :: String -> [GLfloat]
stringToUVs = concatMap charToUVs


stringToVerts :: String -> [Float]
stringToVerts = fst . foldl f acc
    where f (vs,(x,y)) c
              | c == '\n' = (vs, (0, y+1))
              | c == ' '  = (vs, (x+1,y))
              | otherwise = (vs ++ lst x y, (x+1, y))
          acc     = ([], (0.0, 0.0))
          lst x y = [x,y,x+1,y,x+1,y+1,x,y,x+1,y+1,x,y+1]


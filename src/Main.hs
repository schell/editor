{-# LANGUAGE TemplateHaskell #-}
module Main where

import Graphics.UI.GLFW as GLFW
import Data.Maybe
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Control.Monad.Loops
import Control.Lens
import System.Exit
import System.Directory
import Graphics.Rendering.OpenGL hiding (Bitmap, bitmap, Matrix)
import Graphics.Rendering.Text.Renderer
import Graphics.Rendering.Text.Types
import Graphics.Rendering.Shader.Shape as S
import Graphics.Rendering.Shader.Text as T
import Graphics.Cacheing.Text
import Graphics.Math
import Graphics.Utils
import Graphics.Texture.Load
import Editor.Types


data App = App { _textRenderer :: TextRenderer
               , _cursorPos :: (Double,Double)
               , _shapeProgram :: ShapeShaderProgram
               , _zed :: TextureObject
               , _cache :: Maybe RenderCache
               }
makeLenses ''App


main :: IO ()
main = do
    putStrLn "Starting."
    True  <- GLFW.init
    defaultWindowHints
    wvar  <- makeNewWindow (0,0) (742,508) "Font Rendering Is Serious Business"

    -- Make sure our font lives.
    font <- fmap (++ "/fonts/UbuntuMono-B.ttf") getCurrentDirectory
    -- btmp   <- fmap (++ "/assets/text.png") getCurrentDirectory
    exists <- doesFileExist font
    unless exists $ fail $ font ++ " does not exist."

    let load tr = loadCharMap tr testText
    texRenderer <- makeTextRenderer font 16 >>= load
    shapeShader <- makeShapeShaderProgram
    Just zTex <- fmap (++ "/assets/zed.png") getCurrentDirectory >>= flip initTexture 0

    iterateM_ (loop wvar) $ App texRenderer (0,0) shapeShader zTex Nothing


-- | Creates a new window. Fails and crashes if no window can be created.
makeNewWindow :: (Int,Int) -> (Int,Int) -> String -> IO WindowVar
makeNewWindow pos size title = do
    Just win <- uncurry createWindow size title Nothing Nothing
    makeContextCurrent $ Just win
    (uncurry $ setWindowPos win) pos

    mvar <- newMVar ([], win)

    setCharCallback win $ Just $ \_ c ->
        input mvar $ CharEvent c

    setWindowSizeCallback win $ Just $ \_ w h -> do
        print (w,h)
        input mvar $ WindowSizeEvent w h

    setKeyCallback win $ Just $ \_ k i ks modi ->
        input mvar $ KeyEvent k i ks modi

    setMouseButtonCallback win $ Just $ \_ mb mbs modi ->
        input mvar $ MouseButtonEvent mb mbs modi

    setCursorPosCallback win $ Just $ \_ x y ->
        input mvar $ CursorMoveEvent x y

    setCursorEnterCallback win $ Just $ \_ cs ->
        input mvar $ CursorEnterEvent cs

    setScrollCallback win $ Just $ \_ x y ->
        input mvar $ ScrollEvent x y

    return mvar


-- | Inputs some event into a window.
input :: WindowVar -> InputEvent -> IO ()
input mvar e = do
    (es, w) <- takeMVar mvar
    putMVar mvar (e:es, w)


loop :: WindowVar -> App -> IO App
loop wvar app = do
    pollEvents

    -- Get the input this round.
    (es,win) <- takeMVar wvar
    putMVar wvar ([],win)

    (w, h) <- getWindowSize win

    -- Process the input.
    let a = processEvents app es

    makeContextCurrent $ Just win
    -- Render the app in the window.
    cash <- if (null es && (isJust $ _cache a))
              then do
                  threadDelay 10000
                  return $ fromJust $ _cache a
              else do cash <- renderWith a (w, h)
                      swapBuffers win
                      return cash

    -- Quit if need be.
    shouldClose <- windowShouldClose win
    makeContextCurrent Nothing
    when shouldClose exitSuccess
    return a{ _cache = Just cash}


renderWith :: App -> (Int, Int) -> IO RenderCache
renderWith (App t _ s z c) (w, h) = do
    let w' = fromIntegral w
        h' = fromIntegral h
        proj = orthoMatrix 0 w' 0 h' 0 1 :: Matrix GLfloat

    cash <- case c of
        Just cash -> return cash
        Nothing   -> do
            let size = Size (fromIntegral w) (fromIntegral h)
            tex <- renderToTexture size RGB8 $ do
                clearColor $= Color4 0.03 0.17 0.21 1.0
                clear [ColorBuffer, DepthBuffer]
                viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))

                -- Draw a blueish 100x100 square at 10 10
                let vs = quad 10 10 100 100
                    cs = concat $ replicate 6 [0.07,0.21,0.26, 1]
                    vs' = quad (w' - 256) (h' - 256) 256 256
                    uvs = quad 0 0 1 1 :: [GLfloat]
                currentProgram $= (Just $ s^.S.program)
                s^.S.setProjection $ concat proj
                s^.S.setModelview $ concat $ identityN 4
                s^.setIsTextured $ False
                (i,j) <- bindAndBufferVertsColors vs cs
                drawArrays Triangles 0 6
                deleteObjectNames [i,j]

                -- Draw the zed logo.
                s^.setIsTextured $ True
                activeTexture $= TextureUnit 0
                texture Texture2D $= Enabled
                textureBinding Texture2D $= Just z
                s^.S.setSampler $ Index1 0
                (k,l) <- S.bindAndBufferVertsUVs vs' uvs
                drawArrays Triangles 0 6
                deleteObjectNames [k,l]

                -- Draw some text somewhere.
                currentProgram $= (Just $ t^.shader.T.program)
                t^.shader.setTextColor $ Color4 0.52 0.56 0.50 1.0
                t^.shader.T.setProjection $ concat proj
                drawTextAt t (Position 0 0) testText
            return RenderCache { _cacheTexture = tex
                               , _cacheSize = size
                               , _cachePos = Position 0 0
                               }

    clearColor $= Color4 0.03 0.17 0.21 1.0
    clear [ColorBuffer, DepthBuffer]
    viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))

    let Size csw csh = _cacheSize cash
        vs  = quad 0 0 (fromIntegral csw) (fromIntegral csh)
        uvs = texQuad 0 0 1 1

    currentProgram $= (Just $ s^.S.program)
    s^.S.setProjection $ concat proj
    s^.S.setModelview $ concat $ identityN 4
    s^.setIsTextured $ True
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just (_cacheTexture cash)
    s^.S.setSampler $ Index1 0
    (a,b) <- S.bindAndBufferVertsUVs vs uvs
    drawArrays Triangles 0 6
    deleteObjectNames [a,b]

    return cash




processEvents :: App -> [InputEvent] -> App
processEvents a es = a & cursorPos %~ (\cp -> foldr process cp es)
    where process (CursorMoveEvent x y) _ = (x,y)
          process _ cp = cp


testTextOrd :: String
testTextOrd = "A()BCDabcd0123  Hello\nOlé()"

testText :: String
testText = concat $ replicate 20 "Font rendering is...\n  ...serious business.\n"


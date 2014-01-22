module Main where

import Graphics.UI.GLFW as GLFW
import Control.Concurrent.MVar
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
import Graphics.Math
import Graphics.Utils
import Editor.Types


data App = App { _textRenderer :: TextRenderer
               , _cursorPos :: (Double,Double)
               , _shapeProgram :: ShapeShaderProgram
               }


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
    textRenderer <- makeTextRenderer font 16 >>= load

    shapeShader <- makeShapeShaderProgram

    iterateM_ (loop wvar) $ App textRenderer (0,0) shapeShader


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
    let a@(App t _ s) = processEvents app es

    makeContextCurrent $ Just win
    -- Render the app in the window.
    renderWith t s (w, h)
    swapBuffers win

    -- Quit if need be.
    shouldClose <- windowShouldClose win
    makeContextCurrent Nothing
    when shouldClose exitSuccess
    return a


renderWith :: TextRenderer -> ShapeShaderProgram -> (Int, Int) -> IO ()
renderWith t s (w, h) = do
    let w' = fromIntegral w
        h' = fromIntegral h
        proj = orthoMatrix 0 w' 0 h' 0 1 :: Matrix GLfloat

    clearColor $= Color4 0.03 0.17 0.21 1.0
    clear [ColorBuffer, DepthBuffer]
    viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))

    -- Draw a blueish 100x100 square at 10 10
    let vs = quad 10 10 100 100
        cs = concat $ replicate 6 [0.07,0.21,0.26, 1]
    currentProgram $= (Just $ s^.S.program)
    s^.S.setProjection $ concat proj
    s^.S.setModelview $ concat $ identityN 4
    s^.setIsTextured $ False
    (i,j) <- bindAndBufferVertsColors vs cs
    drawArrays Triangles 0 6
    deleteObjectNames [i,j]

    -- Draw some text somewhere.
    currentProgram $= (Just $ t^.shader.T.program)
    t^.shader.setTextColor $ Color4 0.52 0.56 0.50 1.0
    t^.shader.T.setProjection $ concat proj
    drawTextAt' t (0,0) testText


processEvents :: App -> [InputEvent] -> App
processEvents (App tr cp s) es = App tr (foldr process cp es) s
    where process (CursorMoveEvent x y) _ = (x,y)
          process _ a = a


testTextOrd :: String
testTextOrd = "A()BCDabcd0123  Hello\nOlé()"

testText :: String
testText = concat [ "renderWith :: TextRenderer -> ShapeShaderProgram -> (Int, Int) -> IO ()\n"
                  , "renderWith t s (w, h) = do\n"
                  , "    let w' = fromIntegral w\n"
                  , "        h' = fromIntegral h\n"
                  , "        proj = orthoMatrix 0 w' 0 h' 0 1 :: Matrix GLfloat\n"
                  , "\n"
                  , "    clearColor $= Color4 0.03 0.17 0.21 1.0\n"
                  , "    clear [ColorBuffer, DepthBuffer]\n"
                  , "    viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))\n"
                  , "\n"
                  , "    -- Draw a blueish 100x100 square at 10 10\n"
                  , "    let vs = quad 10 10 100 100\n"
                  , "        cs = concat $ replicate 6 [0.07,0.21,0.26, 1]\n"
                  , "    currentProgram $= (Just $ s^.S.program)\n"
                  , "    s^.S.setProjection $ concat proj\n"
                  , "    s^.S.setModelview $ concat $ identityN 4\n"
                  , "    s^.setIsTextured $ False\n"
                  , "    (i,j) <- bindAndBufferVertsColors vs cs\n"
                  , "    drawArrays Triangles 0 6\n"
                  , "    deleteObjectNames [i,j]\n"
                  , "\n"
                  , "    -- Draw some text somewhere.\n"
                  , "    currentProgram $= (Just $ t^.shader.T.program)\n"
                  , "    t^.shader.setTextColor $ Color4 0.52 0.56 0.50 1.0\n"
                  , "    t^.shader.T.setProjection $ concat proj\n"
                  , "    drawTextAt' t (0,0) testText"
                  ]


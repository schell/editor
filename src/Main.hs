module Main where

import Graphics.UI.GLFW as GLFW
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Loops
import Control.Lens
import System.Exit
import System.Directory
import Graphics.Rendering.OpenGL hiding (Bitmap, bitmap, Matrix)
import Graphics.Text.Renderer
import Graphics.Math


data InputEvent = CharEvent Char
                | WindowSizeEvent Int Int
                | KeyEvent Key Int KeyState ModifierKeys
                | MouseButtonEvent MouseButton MouseButtonState ModifierKeys
                | CursorMoveEvent Double Double
                | CursorEnterEvent CursorState
                | ScrollEvent Double Double
                deriving (Show, Eq, Ord)


type WindowVar = MVar ([InputEvent], Window)


data App = App { _textRenderer :: TextRenderer
               , _cursorPos :: (Double,Double)
               }


main :: IO ()
main = do
    putStrLn "Starting."
    True  <- GLFW.init
    defaultWindowHints
    wvar  <- makeNewWindow (0,0) (742,508) "Font Rendering"

    -- Make sure our font lives.
    font <- fmap (++ "/fonts/UbuntuMono-B.ttf") getCurrentDirectory
    -- btmp   <- fmap (++ "/assets/text.png") getCurrentDirectory
    exists <- doesFileExist font
    unless exists $ fail $ font ++ " does not exist."

    let load tr = loadCharMap tr testText
    textRenderer <- initTextRenderer font 16 >>= load

    iterateM_ (loop wvar) $ App textRenderer (0,0)


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
    let a@(App r _) = processEvents app es

    makeContextCurrent $ Just win
    -- Render the app in the window.
    renderWith r (w, h)
    swapBuffers win

    -- Quit if need be.
    shouldClose <- windowShouldClose win
    makeContextCurrent Nothing
    when shouldClose exitSuccess
    return a


renderWith :: TextRenderer -> (Int, Int) -> IO ()
renderWith r (w, h) = do
    let w' = fromIntegral w
        h' = fromIntegral h
        proj = orthoMatrix 0 w' 0 h' 0 1 :: Matrix GLfloat

    clearColor $= Color4 0.03 0.17 0.21 1.0
    clear [ColorBuffer, DepthBuffer]
    viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
    currentProgram $= (Just $ r^.textProgram.tShader.program)
    r^.textProgram.setTextColor $ Color4 0.52 0.56 0.50 1.0
    r^.textProgram.tShader.setProjection $ concat proj
    drawTextAt r (0,0) testText


processEvents :: App -> [InputEvent] -> App
processEvents (App tr cp) = App tr . foldr process cp
    where process (CursorMoveEvent x y) _ = (x,y)
          process _ a = a


testTextOrd :: String
testTextOrd = "A()BCDabcd0123  Hello\nOlé()"

testText :: String
testText = concat [ "[]"
                  , "\n"
                  , "module FontRenderingIsSeriousBusiness where"
                  , "\n"
                  , "\n"
                  , "drawCharacter :: TextRenderer -> (GLfloat, GLfloat) -> Char -> IO (GLfloat, GLfloat)\n"
                  , "drawCharacter r (x,y) char =\n"
                  , "    let mChar = IM.lookup (fromEnum char) $ r^.atlas.atlasMap\n"
                  , "    in\n"
                  , "    case mChar of\n"
                  , "        Nothing -> return (x,y)\n"
                  , "        Just fc@(FontChar tex (w,h) ndx) -> do\n"
                  , "            let p  = r^.textProgram.program\n"
                  , "                Atlas _ ff pxS _ = r^.atlas\n"
                  , "\n"
                  , "            -- Get the metrics about the char.\n"
                  , "            NormGMetrics (bXp,bYp) advp <- fmap normalizeGlyphMetrics $ glyphMetrics ff ndx\n"
                  , "\n"
                  , "            let sW = fromIntegral w\n"
                  , "                sH = fromIntegral h\n"
                  , "                x' = x + sW * realToFrac bXp\n"
                  , "                y' = (y + fromIntegral pxS) - sH * realToFrac bYp\n"
                  , "                a  = realToFrac advp * sW\n"
                  , "                txy = translationMatrix3d x' y' 0\n"
                  , "                sxy = scaleMatrix3d sW sH 1 :: Matrix GLfloat\n"
                  , "                mv  = identityN 4 :: Matrix GLfloat\n"
                  , "                mv' = mv `multiply` txy `multiply` sxy\n"
                  , "\n"
                  , "            texture Texture2D $= Enabled\n"
                  , "            activeTexture $= TextureUnit 0\n"
                  , "            textureBinding Texture2D $= Just tex\n"
                  , "            r^.textProgram.setModelview $ concat mv'\n"
                  , "            r^.quadUVRender\n"
                  , "            return (x + a, y)\n"
                  ]


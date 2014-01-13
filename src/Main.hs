module Main where

import Graphics.UI.GLFW as GLFW
import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Control.Lens
import System.Exit
import System.Directory
import Graphics.Rendering.OpenGL hiding (Bitmap, bitmap, Matrix)
import Graphics.Rendering.FreeType.Internal
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import Graphics.Rendering.FreeType.Internal.Library
import Graphics.Rendering.FreeType.Internal.FaceType
import Graphics.Rendering.FreeType.Internal.Face
import Graphics.Rendering.FreeType.Internal.GlyphSlot
import Graphics.Rendering.FreeType.Internal.Bitmap
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Graphics.Text.Font
import Codec.Picture
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
    wvar  <- makeNewWindow (2,10) (2000,200) "Font Rendering"

    -- Make sure our font lives.
    font <- fmap (++ "/fonts/UbuntuMono-B.ttf") getCurrentDirectory
    -- btmp   <- fmap (++ "/assets/text.png") getCurrentDirectory
    exists <- doesFileExist font
    unless exists $ fail $ font ++ " does not exist."

    textRenderer <- initTextRenderer font 16

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

    setWindowSizeCallback win $ Just $ \win' w h ->
        input mvar $ WindowSizeEvent w h

    setKeyCallback win $ Just $ \win' k i ks mod ->
        input mvar $ KeyEvent k i ks mod

    setMouseButtonCallback win $ Just $ \win' mb mbs mod ->
        input mvar $ MouseButtonEvent mb mbs mod

    setCursorPosCallback win $ Just $ \win' x y ->
        input mvar $ CursorMoveEvent x y

    setCursorEnterCallback win $ Just $ \win' cs ->
        input mvar $ CursorEnterEvent cs

    setScrollCallback win $ Just $ \win' x y ->
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
    let a@(App tr _) = processEvents app es
        w' = fromIntegral w
        h' = fromIntegral h
        proj = orthoMatrix 0 (fromIntegral w) 0 (fromIntegral h) 0 1 :: Matrix GLfloat

    font <- fmap (++ "/fonts/UbuntuMono-R.ttf") getCurrentDirectory
    r <- loadCharMap tr font testText

    -- Render the app in the window.
    makeContextCurrent $ Just win
    clear [ColorBuffer, DepthBuffer]
    viewport $= (Position 0 0, Size w' h')
    currentProgram $= (Just $ r^.textProgram.program)
    r^.setSampler $ Index1 0
    r^.setTextColor $ Color4 1.0 1.0 1.0 1.0
    r^.textProgram.setProjection $ concat proj
    drawTextAt r (0,0) testText
    swapBuffers win

    -- Quit if need be.
    shouldClose <- windowShouldClose win
    makeContextCurrent Nothing
    when shouldClose exitSuccess
    return a{_textRenderer = r}


processEvents :: App -> [InputEvent] -> App
processEvents (App tr cp) = App tr . foldr process cp
    where process (CursorMoveEvent x y) a = (x,y)
          process _ a = a


testText :: String
testText = concat [ "drawCharacter :: TextRenderer -> (GLfloat, GLfloat) -> Char -> IO (GLfloat, GLfloat)\n"
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

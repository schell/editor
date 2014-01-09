module Main where

import Graphics.UI.GLFW as GLFW
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Loops
import System.Exit
import System.Directory
import Graphics.Rendering.FreeType.Internal
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import Graphics.Rendering.FreeType.Internal.Library
import Graphics.Rendering.FreeType.Internal.FaceType
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable


data InputEvent = CharEvent Char
                | WindowSizeEvent Int Int
                | KeyEvent Key Int KeyState ModifierKeys
                | MouseButtonEvent MouseButton MouseButtonState ModifierKeys
                | CursorMoveEvent Double Double
                | CursorEnterEvent CursorState
                | ScrollEvent Double Double
                deriving (Show, Eq, Ord)


type WindowVar = MVar ([InputEvent], Window)


type App = (Double, Double)


main :: IO ()
main = do
    putStrLn "Starting."
    True  <- GLFW.init

    -- Make sure our font lives.
    path   <- fmap (++ "/fonts/UbuntuMono-R.ttf") getCurrentDirectory
    exists <- doesFileExist path
    unless exists $ fail $ path ++ " does not exist."

    -- FreeType (http://freetype.org/freetype2/docs/tutorial/step1.html)
    ft <- freeType
    -- Get the Ubuntu Mono fontface.
    ff <- fontFace ft path
    runFreeType $ ft_Set_Pixel_Sizes ff 16 16
    -- Get the unicode char index.
    chNdx <- ft_Get_Char_Index ff 65
    -- Load the glyph into freetype memory.
    runFreeType $ ft_Load_Glyph ff chNdx 0

    wvar  <- makeNewWindow (100,100) (800,800) "Title"
    iterateM_ (loop wvar) (0,0)


runFreeType :: IO FT_Error -> IO ()
runFreeType m = do
    r <- m
    unless (r == 0) $ fail $ "FreeType Error:" ++ show r


freeType :: IO FT_Library
freeType = alloca $ \p -> do
    runFreeType $ ft_Init_FreeType p
    peek p


fontFace :: FT_Library -> FilePath -> IO FT_Face
fontFace ft fp = withCString fp $ \str ->
    alloca $ \ptr -> do
        runFreeType $ ft_New_Face ft str 0 ptr
        peek ptr


-- | Creates a new window. Fails and crashes if no window can be created.
makeNewWindow :: (Int,Int) -> (Int,Int) -> String -> IO WindowVar
makeNewWindow pos size title = do
    Just win <- uncurry createWindow size title Nothing Nothing
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
    (es,w) <- takeMVar wvar
    putMVar wvar ([],w)

    -- Process the input.
    let app' = processEvents app es

    -- Render the app in the window.
    makeContextCurrent $ Just w
    --print app'

    -- Quit if need be.
    shouldClose <- windowShouldClose w
    makeContextCurrent Nothing
    when shouldClose exitSuccess
    return app'

processEvents :: App -> [InputEvent] -> App
processEvents = foldr process
    where process (CursorMoveEvent x y) a = (x,y)
          process _ a = a

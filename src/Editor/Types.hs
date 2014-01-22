module Editor.Types where

import           Graphics.UI.GLFW
import           Control.Concurrent.MVar
import           Data.Monoid
import           Graphics.Rendering.Text.Types


data InputEvent = CharEvent Char
                | WindowSizeEvent Int Int
                | KeyEvent Key Int KeyState ModifierKeys
                | MouseButtonEvent MouseButton MouseButtonState ModifierKeys
                | CursorMoveEvent Double Double
                | CursorEnterEvent CursorState
                | ScrollEvent Double Double
                deriving (Show, Eq, Ord)


type WindowVar = MVar ([InputEvent], Window)


data Editor = Editor { _editorWindowVar     :: Maybe WindowVar
                     , _editorTextRenderers :: [TextRenderer]
                     }

instance Monoid Editor where
    mempty = Editor Nothing []
    mappend (Editor v b) (Editor _ d) = Editor v (b `mappend` d)


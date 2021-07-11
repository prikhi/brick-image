{- |

This module is used to interact with the `w3mimgdisplay` terminal image
rendering program provided by the `w3m` CLI web browser.

You can use this to render actual images in a terminal instead of
colorized, ASCII-art versions of images.

If you simply want to show an image, take a look at the `showImage`
function. If you want more control over the messages sent to the
`w3mimgdisplay` process, see the `Command` type & `runCommands` functions.

TODO:

* high-level: forkable server that starts process, reads commands from
channel, sends updates to process via stdin, & closes up when done.

-}
{-# LANGUAGE RecordWildCards #-}
module W3mImgDisplay
    ( isW3mImgDisplayAvailable
    , showImage
    , runCommands
    , runCommand
    , Command(..)
    , ImageParams(..)
    , ClearParams(..)
    , Position(..)
    , Dimensions(..)
    )
    where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default (Default(def))
import Data.List (intercalate)
import Data.Maybe (isJust)
import System.Directory (findExecutable, canonicalizePath)
import System.Process.Typed (proc, setStdin, setStdout, byteStringOutput, createPipe, withProcessTerm, getStdin)
import System.IO (hPutStrLn, hClose)


-- | Determine if the `w3mimgdisplay` program is available & executable.
isW3mImgDisplayAvailable :: MonadIO m => m Bool
isW3mImgDisplayAvailable =
    isJust <$> liftIO (findExecutable "w3mimgdisplay")


-- HIGH LEVEL

-- | Show an Image at the Position, Scaled to the Dimensions.
showImage :: MonadIO m => FilePath -> Position -> Dimensions -> m ()
showImage fp position dimensions = do
    imagePath <- liftIO $ canonicalizePath fp
    runCommands
        [ DrawImage ImageParams
            { iID = 1
            , iPosition = position
            , iSourceDimensions = dimensions
            , iShownOffset = def
            , iShownDimensions = def
            , iPath = imagePath
            }
        , SyncCommunication
        , SyncDrawing
        ]




-- LOW LEVEL

-- | Send a series of Commands to a single w3mimgdisplay Process.
runCommands :: MonadIO m => [Command] -> m ()
runCommands =
    runWithStdIn . concatMap show

-- | Run a single W3mImgDisplay Command.
runCommand :: MonadIO m => Command -> m ()
runCommand c =
    runWithStdIn $ show c

runWithStdIn :: MonadIO m => String -> m ()
runWithStdIn commands =
    let
        config =
            setStdin createPipe
                $ setStdout byteStringOutput
                $ proc "w3mimgdisplay" []
    in
        liftIO . withProcessTerm config $ \p -> do
            let stdin = getStdin p
            hPutStrLn stdin commands
            hClose stdin

-- | A `Command` is one of the actions that `w3mimgdisplay` accepts.
data Command
    = DrawImage ImageParams
    | RedrawImage ImageParams
    | TerminateDrawing
    | SyncDrawing
    | SyncCommunication
    | GetSize FilePath
    | ClearImage ClearParams
    deriving (Eq)

instance Show Command where
    show c =
        show opCode ++ ";" ++ parameters ++ "\n"
        where
            opCode :: Integer
            opCode = case c of
                DrawImage _ ->
                    0
                RedrawImage _ ->
                    1
                TerminateDrawing ->
                    2
                SyncDrawing ->
                    3
                SyncCommunication ->
                    4
                GetSize _ ->
                    5
                ClearImage _ ->
                    6
            parameters :: String
            parameters = case c of
                DrawImage ips ->
                    show ips
                RedrawImage ips ->
                    show ips
                GetSize fp ->
                    fp
                ClearImage cps ->
                    show cps
                TerminateDrawing ->
                    ""
                SyncDrawing ->
                    ""
                SyncCommunication ->
                    ""


-- | Potential X & Y Coordinates.
data Position
    = Position
        { _x :: Maybe Int
        , _y :: Maybe Int
        } deriving (Eq)

instance Default Position where
    def = Position Nothing Nothing

-- | Potential Width & Height of an Area.
data Dimensions
    = Dimensions
        { width :: Maybe Int
        , height :: Maybe Int
        } deriving (Eq)

instance Default Dimensions where
    def = Dimensions Nothing Nothing

-- | w3mimgdisplay works by opening a file into a pixmap, then copying this
-- pixmap to a position.
--
-- The size of the pixmap is set by `iSourceDimensions`, while the size of
-- the area it is copied to is set by `iShownDimensions`. Only pixels below
-- and to the right of the `iShownOffset` `Position` is rendered.
data ImageParams
    = ImageParams
        { iID :: Int
        -- ^ A Unique ID for the image
        , iPosition :: Position
        -- ^ The position to draw the image at.
        -- Defaults to (0, 0)
        , iSourceDimensions :: Dimensions
        -- ^ The dimensions of the pixmap to load the images into.
        -- Defaults to the image resolution.
        , iShownOffset :: Position
        -- ^ The offset in the buffer to start rendering at.
        -- Defaults to (0, 0)
        , iShownDimensions :: Dimensions
        -- ^ The dimensions to render to the screen.
        -- Defaults to the dimensions specified in `iSourceDimensions`.
        , iPath :: FilePath
        -- ^ The image file to display
        }
    deriving (Eq)

-- Delimit the arguments w/ semi-colons
instance Show ImageParams where
    show ImageParams {..} =
        intercalate ";" $
            [ show iID
            , maybeShow _x iPosition
            , maybeShow _y iPosition
            , maybeShow width iSourceDimensions
            , maybeShow height iSourceDimensions
            , maybeShow _x iShownOffset
            , maybeShow _y iShownOffset
            , maybeShow width iShownDimensions
            , maybeShow height iShownDimensions
            ]
            ++ [ iPath ]

-- | Clear the rectangle defined by the `Position` & `Dimensions`.
data ClearParams
    = ClearParams
        { cOffset :: Position
        -- ^ The positon to start clearing at. Defaults to (0, 0)
        , cDimensions :: Dimensions
        -- ^ The dimensions to clear. Defaults to the remaining space below
        -- and to the right of `cOffset`.
        }
    deriving (Eq)

instance Show ClearParams where
    show ClearParams {..} =
        intercalate ";"
            [ maybeShow _x cOffset
            , maybeShow _y cOffset
            , maybeShow width cDimensions
            , maybeShow height cDimensions
            ]

maybeShow :: Show b => (a -> Maybe b) -> a -> String
maybeShow f = maybe "" show . f

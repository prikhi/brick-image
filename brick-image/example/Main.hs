{-# LANGUAGE RecordWildCards #-}
module Main where

import Brick
import Brick.Widgets.List
import Control.Monad (void)
import Data.Char (toLower)
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Directory

import qualified Brick.Widgets.Border as B
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

import Brick.Widgets.Image


data AppWidget
    = FileList
    | ImagePreview
    deriving (Eq, Ord, Show)

data AppState
    = AppState
         { fileList :: List AppWidget FilePath
         , longestFileName :: Int
         , imagePreview :: ImageWidget AppWidget
         }

data AppEvent

main :: IO ()
main = do
    state <- initialize
    void $ defaultMain app state

initialize :: IO AppState
initialize = do
    files <- filter isImageFile <$> (getCurrentDirectory >>= listDirectory)
    let fileList = list FileList (Vec.fromList files) 1
        firstFile = fromMaybe "" $ listToMaybe files
        longestFileName = maximum $ map length files
    imagePreview <- imageWidgetIO ImagePreview firstFile
    return AppState {..}

app :: App AppState AppEvent AppWidget
app =
    (simpleApp undefined)
        { appDraw = draw
        , appHandleEvent = handleEvent
        , appAttrMap = const $
            attrMap V.defAttr
                [ (listSelectedAttr, V.black `on` V.white) ]
        }

draw :: AppState -> [Widget AppWidget]
draw AppState {..} =
    [ B.borderWithLabel (str " Image Preview ") $ hBox
        [ hLimit (longestFileName + 1) $ renderList renderFileName True fileList
        , renderImageWidget imagePreview
        ]
    ]
    where
        renderFileName _ =
            padRight Max . str

handleEvent :: AppState -> BrickEvent AppWidget AppEvent -> EventM AppWidget (Next AppState)
handleEvent s e = case e of
    VtyEvent (V.EvKey (V.KChar 'q') []) ->
        halt s
    VtyEvent ev -> do
        fileList <- handleListEventVi handleListEvent ev (fileList s)
        imagePreview <- case listSelectedElement fileList of
            Just (_, fp) ->
                if isImageFile fp then
                    changeFilePath fp (imagePreview s)
                        >>= handleImageWidgetEvent ev
                else
                    return $ imagePreview s
            Nothing ->
                return $ imagePreview s
        continue s { fileList = fileList, imagePreview = imagePreview }
    _ ->
        continue s

isImageFile :: FilePath -> Bool
isImageFile fp =
    any (`isSuffixOf` map toLower fp) ["jpg", "png", "jpeg", "bmp", "gif"]

{- |

This module contains an `ImageWidget` Brick widget which allows you to
embed an actual image into a Brick application, instead of the colorized,
ASCII renderings you usually see in terminals.

It uses the `w3mimgdisplay` program that comes with the `w3m` CLI web
browser. You should have `w3m` installed if you want this module to
actually do something.

TODO: write module example/documentation, base off of Brick.Widgets.List

-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
module Brick.Widgets.Image
    ( ImageWidget
    , imageWidgetIO
    , handleImageWidgetEvent
    , changeFilePath
    , renderImageWidget
    ) where

import Brick
import Control.Arrow (first, second)
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Conduit ((.|), runConduitRes)
import Data.Conduit.Combinators (sourceFile)
import Data.Conduit.ImageSize (Size(..), sinkImageSize)
import Data.Ratio ((%))

import qualified Graphics.Vty as V

import W3mImgDisplay

-- | TODO: Export functions for getting path/dimensions from widget.
-- TODO: Positioning types/functions for placing image in available area
--       (e.g., Top, Left, Center, Middle)? If we can't get widget
--       shrinking to work...
data ImageWidget n
    = ImageWidget
        { name :: n
        , imagePath :: FilePath
        , imageDimensions :: (Int, Int)
        }

instance Named (ImageWidget n) n where
    getName = name


{- | Construct a new `ImageWidget`. This needs to be in IO so we can
determine the image's dimensions.
-}
imageWidgetIO :: MonadIO m => n -> FilePath -> m (ImageWidget n)
imageWidgetIO n path = do
    dimensions <- getDimensions path
    return ImageWidget
        { name = n
        , imagePath = path
        , imageDimensions = dimensions
        }

getDimensions :: MonadIO m => FilePath -> m (Int, Int)
getDimensions path =
    either (const (0, 0)) (\(Size w h) -> (w, h))
        <$> liftIO (runConduitRes $ sourceFile path .| sinkImageSize)


-- | pull viewport extent - re-render on resize, lost focus, gained focus
handleImageWidgetEvent :: Ord n => V.Event -> ImageWidget n -> EventM n (ImageWidget n)
handleImageWidgetEvent e i = flip (>>) (return i) $ case e of
    V.EvResize _ _ ->
        imageDisplay i
    V.EvLostFocus ->
        liftIO (putStrLn "Lost Focus") >>
        imageDisplay i
    V.EvGainedFocus ->
        liftIO (putStrLn "Gained Focus") >>
        imageDisplay i
    _ ->
        return ()


-- | fetch dimensions, re-render, return new widget
changeFilePath :: Ord n => FilePath -> ImageWidget n -> EventM n (ImageWidget n)
changeFilePath path i = do
    dimensions <- getDimensions path
    let i_ = i { imagePath = path, imageDimensions = dimensions }
    imageDisplay i_
    return i_

-- | TODO: Decouple pixel calcualtions from my terminal font
imageDisplay :: Ord n => ImageWidget n -> EventM n ()
imageDisplay ImageWidget {..} = do
    maybeExtent <- lookupExtent name
    case maybeExtent of
        Just extent -> do
            let extentPixelSize =
                    first (* 7) . second (* 13) $ extentSize extent
                renderPosition =
                    fromTuple Position (* 7) (* 13)
                        $ loc $ extentUpperLeft extent
                renderDimensions =
                    fromTuple Dimensions id id
                        $ calculateDimensions extentPixelSize imageDimensions
            runCommands
                [ ClearImage
                    $ ClearParams
                        renderPosition
                        (fromTuple Dimensions id id extentPixelSize)
                , SyncCommunication
                , SyncDrawing
                ]
            showImage imagePath renderPosition renderDimensions
        Nothing ->
            return ()
    where
        fromTuple constr f g =
            uncurry constr . first (Just . f) . second (Just . g)
        calculateDimensions (aW, aH) (iW, iH)
            | iW == 0 || iH == 0 =
                (aW, aH)
            | aW >= iW && aH >= iH =
                (iW, iH)
            | aW >= iW =
                scaleToHeight
            | aH >= iH =
                scaletoWidth
            | (iH - aH) > (iW - aW) =
                scaleToHeight
            | otherwise =
                scaletoWidth
            where
                scaleToHeight =
                    ( round $ aH * iW % iH
                    , aH
                    )
                scaletoWidth =
                    ( aW
                    , round $ aW * iH % iW
                    )



{- | Render the empty space the image will take up.

Originally, the size of this widget would change when the image changed
- but new images were being shown at the sizes of previous images.

So now we just fill all available size & do dimension comparison in
`imageDisplay`.

But it would be cool to have the widet shrink to the actual dimensions of
the rendered image.
-}
renderImageWidget :: (Ord n, Show n) => ImageWidget n -> Widget n
renderImageWidget i =
    Widget Greedy Greedy $ do
        context <- getContext
        let (width, height) =
                (context ^. availWidthL - 1 , context ^. availHeightL - 1)
        render
            $ hLimit width
            $ vLimit height
            $ viewport (getName i) Both
            $ hLimit width
            $ vLimit height
            $ fill ' '
-- My Original Crop Area to Context Approach:
-- TODO: explore this approach more
        --
        --let (width, height) =
        --        calculateDimensions
        --            ( context ^. availWidthL . to (* 7)
        --            , context ^. availHeightL . to (* 13)
        --            )
        --            $ imageDimensions i
        --render
        --    $ hLimit (floor $ width % 7)
        --    $ vLimit (floor $ height % 13)
        --    $ viewport (getName i) Both
        --    $ hLimit (floor $ width % 7)
        --    $ vLimit (floor $ height % 13)
        --    $ fill ' '
    --where
    --    calculateDimensions (aW, aH) (iW, iH)
    --        | iW == 0 || iH == 0 =
    --            (aW, aH)
    --        | aW >= iW && aH >= iH =
    --            (iW, iH)
    --        | aW >= iW =
    --            scaleToHeight
    --        | aH >= iH =
    --            scaletoWidth
    --        | (iH - aH) > (iW - aW) =
    --            scaleToHeight
    --        | otherwise =
    --            scaletoWidth
    --        where
    --            scaleToHeight =
    --                ( floor $ aH * iW % iH
    --                , aH
    --                )
    --            scaletoWidth =
    --                ( aW
    --                , floor $ aW * iH % iW
    --                )

# brick-image

[![brick-image's Travis-CI Build Status](https://travis-ci.org/prikhi/brick-image.svg?branch=master)](https://travis-ci.org/prikhi/brick-image "brick-image on travis-ci.org")

Render raw images as [Brick](https://github.com/jtdaugherty/brick) widgets
using [w3mimgdisplay](https://github.com/tats/w3m/blob/master/w3mimgdisplay.c).

![A screenshot of a terminal running the example brick-image application, showing a list of files and an image preview of the currently selected file.](/screenshot.png?raw=true "brick-image Example Image Viewer")

This is a super-rough, buggy first attempt at making this work.

You will need to have `w3m` installed.

You can use the `W3mImgDisplay` module to render images yourself, and the
`Brick.Widgets.Image` module to render an image in Brick.

Both the manual rendering & brick rendering are working fine. However,
transformation from Brick's position/size data to pixels is currently hardcoded
for my terminal font. And the image isn't always completed cleaned up when
changed.

There is a small example app in `example/` that implements an Image Viewer for
the current directory using Image & List widgets:

    stack build
    # Assuming you have image files in ~/Pictures
    cp ~/Pictures/*.jpg .
    stack exec brick-image-example


## TODO

Comments, design feedback, PRs, & bug reports are all appreciated.

Decouple the row/column->pixel conversion from my terminal font:

* Quick, hacky version: `xrdb -q | grep -i font` will let us grab the font from
  xresources, `XLoadQueryFont` will let us grab size details.
* See xterm control sequence `\e[14t`:
  http://invisible-island.net/xterm/ctlseqs/ctlseqs.html
* Also `xtermctl` or terminfo
  https://stackoverflow.com/questions/22782703/how-to-get-terminal-size-or-font-size-in-pixels
* The `w3mimgdisplay -test` command returns the width & height of the terminal
  in pixels. Maybe use that if we can get total rows/columns from Vty module?

Figure out when we should be using the `RedrawImage` command.

Fix the brick widget rendering - when side-by-side w/ a list widget that
changes the selected items color, a line will be missing from the image
whenever the list is scrolled. This is visible in the example app. I was able
to fix this by adding a delay before calling w3mimgdisplay `forkIO $
threadDelay 50000 >> imageDisplay i` but that seems like a very hacky solution.

Fix the brick widget clearing - sometimes a tiny sliver of the bottom or left
of the image doesn't get cleared when changing the file path. Probably requires
sending `TerminateDrawing` via the "server" described below. This is visible in
the example app.

Fix image rendering on startup. Images aren't displayed on app startup becaue
their viewports have no extents yet. Not sure how to work around this, seems
like we'd need to trigger an update after the initial rendering... This is
visible in the example app.

Work on the library interface, it's currently just the minimum I needed to make it
work.

Have `W3mImgDisplay` implement a "server" that keeps a long-running
w3mimgdisplay process open & feeds it Commands received from a Chan or
TChan(might need this for clearing images correctly).
Eventually it could have a really high-level interface that manages images &
moving/clearing/layering them, etc.

Figure out potential arguments by inspecting `w3mimgdisplay` source - add
`Options` or `Config` type & `runWithOptions` that allows library users to pass
arguments to launched process.

Module/usage documentation.

Tests

    * ensure command output has correct number of fields
    * ensure setting parameter sets correct output field

Stick un-exported functions into `Internal` modules?

Rename github repo to `hs-w3mimgdisplay`.

Release on hackage/stackage once working properly

    * http://taylor.fausak.me/haskell-package-checklist/
    * http://fvisser.nl/post/2013/may/28/towards-a-better-haskell-package.html
    * https://wiki.haskell.org/How_to_write_a_Haskell_program
    * How to release on hackage & stackage?

Lofty goals would be eventually re-implementing w3mimgdisplay in Haskell.


## LICENSE

GPL-3.0, but I could loosen it up if it means more people get to use this.

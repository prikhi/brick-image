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

Decouple the row/column->pixel conversion from my terminal font.

Figure out when we should be using the `RedrawImage` command.

Figure out why Brick doesn't trigger lost/gained focus events for me. Is it
urxvt or xmonad?

Fix the brick widget rendering - when side-by-side w/ a list widget that
changes the selected items color, a line will be missing from the image
whenever the list is scrolled. This is visible in the example app.

Fix the brick widget clearing - sometimes a tiny sliver of the bottom or left
of the image doesn't get cleared when changing the file path. Probably requires
sending `TerminateDrawing` via the "server" described below. This is visible in
the example app.

Work on the library interface, it's currently just the minimum I needed to make it
work.

Have `W3mImgDisplay` implement a "server" that keeps a long-running
w3mimgdisplay process open & feeds it Commands received from a Chan or TChan.
Maybe it could manage moving images, clearing specific images, etc. This might
be required for perfectly cleaning up an image.

Figure out potential arguments by inspecting `w3mimgdisplay` source - add
`Options` or `Config` type & `runWithOptions` that allows library users to pass
arguments to launched process.

Module/usage documentation.

Tests

    * ensure command output has correct number of fields
    * ensure setting parameter sets correct output field

Stick un-exported functions into `Internal` modules.

Split into a `w3mimgdisplay` package & a `brick-image` package.

Release on hackage/stackage once working properly

    * http://taylor.fausak.me/haskell-package-checklist/
    * http://fvisser.nl/post/2013/may/28/towards-a-better-haskell-package.html
    * https://wiki.haskell.org/How_to_write_a_Haskell_program
    * How to release on hackage & stackage?


## LICENSE

GPL-3.0, but I could loosen it up if it means more people get to use this.

module Pager where
import System (getArgs)
import Graphics.Vty
import Graphics.Vty.Attributes
import Data.Word
import System.IO
import FileOps
import Text.Printf

pageFile f = do
    vt <- mkVty
    DisplayRegion w h <- display_bounds $ terminal vt
    contents <- readFile f
    showInPager vt 0 w h f contents

showInPager vt y sx sy header fstr = do
    let stopPlay      = shutdown vt >> return ()
        lastPage      = length stxt < fromIntegral sy
        pageIncrement = if lastPage
                           then stopPlay
                           else showInPager vt (y + 1) sx (sy-2) header fstr
        pageDecrement = if (y <= 0)
                           then refreshPage
                           else showInPager vt (y - 1) sx (sy+2) header fstr
        refreshPage   = showInPager vt y sx (sy) header fstr
        defaultAction = if lastPage then stopPlay else refreshPage
        stxt          = getPage (fromIntegral sx) (fromIntegral sy) y fstr

    update vt (currentPic header (fromIntegral (sy-1) * y) stxt)
    k <- next_event vt
    case k of EvKey (KASCII ' ') [] -> pageIncrement
              EvKey KDown        [] -> pageIncrement
              EvKey KUp          [] -> pageDecrement
              EvKey KEsc         [] -> stopPlay
              EvKey (KASCII 'q') [] -> stopPlay
              EvResize nx ny        -> showInPager vt (min y (toEnum ny - 2))
                                               (toEnum nx) (toEnum ny) header fstr
              _                     -> defaultAction

currentPic header offset lines =
    let hAttr      = Attr (SetTo bold) (SetTo green) (SetTo bright_black)
        nAttr      = Attr (SetTo bold) (SetTo yellow) (SetTo black)
        spacer     = string def_attr " "
        digitWidth = length . show
        numFmt :: Int -> String
        numFmt n   = printf "%0*d"
                        (digitWidth (offset + length lines)) (offset + n)
    in

    pic_for_image $ string hAttr header <-> spacer <->
        vert_cat (zipWith (\x y -> string nAttr (numFmt y) <|>
            spacer <|> string def_attr x) lines [1..])

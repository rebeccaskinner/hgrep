module Pager where
import System.Environment (getArgs)
import Graphics.Vty
import Graphics.Vty.Prelude
import Graphics.Vty.Attributes
import Data.Default (def)
import Data.Word
import System.IO
import FileOps
import Text.Printf

pageFile f = do
    vt <- mkVty def
    (w,h) <- displayBounds $ outputIface vt
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
    k <- nextEvent vt
    case k of EvKey (KChar ' ') [] -> pageIncrement
              EvKey KDown        [] -> pageIncrement
              EvKey KUp          [] -> pageDecrement
              EvKey KEsc         [] -> stopPlay
              EvKey (KChar 'q') [] -> stopPlay
              EvResize nx ny        -> showInPager vt (min y (toEnum ny - 2))
                                               (toEnum nx) (toEnum ny) header fstr
              _                     -> defaultAction

currentPic header offset lines =
    let hAttr      = Attr (SetTo bold) (SetTo green) (SetTo brightBlack)
        nAttr      = Attr (SetTo bold) (SetTo yellow) (SetTo black)
        spacer     = string defAttr " "
        digitWidth = length . show
        numFmt :: Int -> String
        numFmt n   = printf "%0*d"
                        (digitWidth (offset + length lines)) (offset + n)
    in

    picForImage $ string hAttr header <-> spacer <->
        vertCat (zipWith (\x y -> string nAttr (numFmt y) <|>
            spacer <|> string defAttr x) lines [1..])

module Pager where
import System (getArgs)
import Graphics.Vty
import Graphics.Vty.Attributes
import Data.Word
import System.IO
import FileOps

pageFile f = do
    vt <- mkVty
    DisplayRegion w h <- display_bounds $ terminal vt
    contents <- readFile f
    showInPager vt 0 w h f contents

showInPager vt y sx sy header fstr = do
    let stopPlay      = shutdown vt >> return ()
        lastPage      = length stxt < (fromIntegral sy)
        pageIncrement = if lastPage then stopPlay else showInPager vt (y + 1) sx sy header fstr
        pageDecrement = showInPager vt (y - 1) sx sy header fstr
        refreshPage   = showInPager vt y sx sy header fstr
        defaultAction = if lastPage then stopPlay else refreshPage
        stxt          = getPage (fromIntegral sx) (fromIntegral sy) y fstr

    update vt (current_pic header stxt)
    k <- next_event vt
    case k of EvKey (KASCII ' ') [] -> pageIncrement
              EvKey KDown        [] -> pageIncrement
              EvKey KUp          [] -> pageDecrement
              EvKey KEsc         [] -> stopPlay
              EvKey (KASCII 'q') [] -> stopPlay
              EvResize nx ny        -> showInPager vt (min y (toEnum ny - 2))
                                               (toEnum nx) (toEnum ny) header fstr
              _                     -> defaultAction

current_pic header lines =
    let hAttr = Attr (SetTo bold) (SetTo green) (SetTo bright_black) in
    pic_for_image $
    string hAttr header <->
        string def_attr "\n" <->
        (vert_cat ( map (string def_attr) lines))

-- main = do
--     getArgs >>= (return . head) >>= pageFile

-- module Pager where
import System (getArgs)
import Graphics.Vty
import Data.Word
import System.IO
import FileOps

fi = fromIntegral

main = do
    vt <- mkVty
    fstr <- (getArgs >>= (return . head) >>= readFile)
    DisplayRegion w h <- display_bounds $ terminal vt
    putStrLn $ show $ DisplayRegion w h
    showInPager vt 0 w h fstr

showInPager vt y sx sy fstr = do
    let stopPlay      = shutdown vt >> return ()
        pageIncrement = if [] == stxt then stopPlay else showInPager vt (y + 1) sx sy fstr
        pageDecrement = showInPager vt (y - 1) sx sy fstr
        stxt          = getPage (fi sx) (fi sy) y fstr

    update vt (current_pic stxt)
    k <- next_event vt
    case k of EvKey (KASCII ' ') [] -> pageIncrement
              EvKey KDown        [] -> pageIncrement
              EvKey KUp          [] -> pageDecrement
              EvKey KEsc         [] -> stopPlay
              EvKey (KASCII 'q') [] -> stopPlay
              EvResize nx ny        -> showInPager vt (min y (toEnum ny - 2))
                                               (toEnum nx)
                                               (toEnum ny)
                                               fstr
              _                     -> showInPager vt y sx sy fstr

current_pic lines = pic_for_image $ vert_cat $ map (string def_attr) lines

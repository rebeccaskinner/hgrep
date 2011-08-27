import Graphics.Vty
import qualified Data.ByteString as B
import Data.Word
import System.IO
import FileOps

testFile = do readFile "/home/miyako/Documents/Book/TimeOfDeath/chapter1.txt"

main = do
    vt <- mkVty
    DisplayRegion w h <- display_bounds $ terminal vt
    putStrLn $ show $ DisplayRegion w h
    play vt 0 w h " "

play vt y sx sy btl = do
    stxt <- testFile 
    let lines = take (fromIntegral sy) $ drop (min y ((length stxt) - (fromIntegral sy))) $ hardWrapLines (fromIntegral sx) stxt
    update vt (current_pic lines)
    k <- next_event vt
    case k of EvKey (KASCII ' ') [] -> play vt (y + fromIntegral sy) sx sy btl
              EvKey KEsc []         -> shutdown vt >> return ()
              EvKey (KASCII 'q') [] -> shutdown vt >> return ()
              EvResize nx ny        -> play vt (min y (toEnum ny - 2))
                                               (toEnum nx)
                                               (toEnum ny)
                                               btl
              _                     -> play vt y sx sy (take (fromEnum sx) (show k ++ btl))

current_pic lines = pic_for_image $ vert_cat $ map (string def_attr) lines

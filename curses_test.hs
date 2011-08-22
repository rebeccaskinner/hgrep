import Graphics.Vty
import qualified Data.ByteString as B
import Data.Word
import System.IO

testFile = do readFile "/home/miyako/Documents/Book/TimeOfDeath/chapter1.txt"

main = do
    vt <- mkVty
    DisplayRegion w h <- display_bounds $ terminal vt
    putStrLn $ show $ DisplayRegion w h
    play vt 0 1 w h ""

play vt x y sx sy btl = do
    stxt <- testFile
    putStrLn "Test File"
    putStrLn stxt
    putStrLn "---"
    update vt (current_pic stxt)
    k <- next_event vt
    case k of EvKey (KASCII ' ') [] -> play vt x (y+1) sx sy btl
              EvKey KEsc []         -> shutdown vt >> return ()
              EvResize nx ny        -> play vt (min x (toEnum nx - 1))
                                               (min y (toEnum ny - 2))
                                               (toEnum nx)
                                               (toEnum ny)
                                               btl
              _                     -> play vt x y sx sy (take (fromEnum sx) (show k ++ btl))

current_pic stxt = pic_for_image $ string def_attr stxt

import Data.Time
import Data.Time.Format
import Control.Monad
import System.Posix.Unistd
import TermSize
import System.IO

ecma_code x c = 
    putStr ("\x9B\&" ++ show x ++ c)

clear_screen =
    ecma_code 2 "J"

cursor_up x = 
    ecma_code x "A"

cursor_down x = 
    ecma_code x "B"

cursor_right x = 
    ecma_code x "C"

cursor_left x = 
    ecma_code x "D"

cursor_to_column x = 
    ecma_code x "G"

cursor_to x y = 
    putStr ("\x9B\&" ++ show x ++ ";" ++ show y ++ "f")

cursor_bottom_middle = do 
    (nrow, ncol) <- getTermSize
    cursor_to (nrow - 1) (ncol `div` 2)


putStr_centered string = do
    cursor_left (length string `div` 2)
    putStr string

equinor_logo = do 
    (nrow, ncol) <- getTermSize
    cursor_to 0 ncol
    cursor_left 16
    putStr "               ▖"
    cursor_down 1
    cursor_left 16
    putStr "         ▟▙ ▗▟█▎"
    cursor_down 1
    cursor_left 16
    putStr "         ▜▛ ▜█▛ "
    cursor_down 1
    cursor_left 16
    putStr "         -  -   "
    cursor_down 1
    cursor_left 16
    putStr "        █  ▚    " 
    cursor_down 1
    cursor_left 16
    putStr "equinor▝        "
    
draw_loop draw = do
    clear_screen
    draw
    forever (do {
      (old_nrow, old_ncol) <- getTermSize;
      draw;
      sleep 1;
      (new_nrow, new_ncol) <- getTermSize;
      if new_nrow /= old_nrow then clear_screen else return ();
      if new_ncol /= old_ncol then clear_screen else return ();
      })

present = do 
    equinor_logo
    cursor_bottom_middle
    (putStr_centered . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" =<< getCurrentTime )

main = do 
    hSetBuffering stdout NoBuffering
    draw_loop present

import System.IO  
import qualified Data.Text as T

testFileName = "../inputs/input-6.0.txt"
  
main = do  
    handle <- openFile testFileName ReadMode  
    contents <- hGetContents handle  
    print $ readlines contents  
    hClose handle  

readlines :: String -> [String]
readlines contents = 
    map T.unpack $ T.splitOn (T.pack "\n") (T.pack contents)
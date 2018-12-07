import System.IO  
import qualified Data.Text as T
import qualified Data.Map as Map

testFileName = "../inputs/input-7.0.txt"
fileName = "../inputs/input-7.1.txt"

main = do  
    handle <- openFile testFileName ReadMode  
    contents <- hGetContents handle  
    let vals = map condenseDependency (readlines contents)
    print $ aggregateDependencies vals
    hClose handle  

readlines :: String -> [String]
readlines contents = 
    map T.unpack $ T.splitOn (T.pack "\n") (T.pack contents)


condenseDependency :: String -> [String]
condenseDependency dependencyStr =
    filter (\x -> (length x) == 1) (map T.unpack (T.splitOn (T.pack " ") (T.pack dependencyStr)))


aggregateDependencies :: [[String]] -> [(String, [String])]
aggregateDependencies dependencies = 
    let reversed = map reverse dependencies in
        map (\x -> (head x, tail x)) reversed
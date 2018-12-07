import System.IO  
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set

testFileName = "../inputs/input-7.0.txt"
fileName = "../inputs/input-7.1.txt"

main = do  
    handle <- openFile testFileName ReadMode  
    contents <- hGetContents handle  
    let vals = map condenseDependency (readlines contents)
    let aggDeps = aggregateDependencies vals
    let steps = allSteps vals
    print $ steps
    print $ aggDeps
    hClose handle  

readlines :: String -> [String]
readlines contents = 
    map T.unpack $ T.splitOn (T.pack "\n") (T.pack contents)


condenseDependency :: String -> [String]
condenseDependency dependencyStr =
    filter (\x -> (length x) == 1) (map T.unpack (T.splitOn (T.pack " ") (T.pack dependencyStr)))

aggregateDependencies :: [[String]] -> Map.Map String [String]
aggregateDependencies dependencies = 
    let deps = foldl buildAggregate Map.empty dependencies in
        let stepsWithDeps = map fst (Map.toList deps) in
            let all = allSteps dependencies in
                let stepsWithoutDeps = all List.\\ stepsWithDeps in
                    foldl (\acc x -> Map.insert x [] acc) deps stepsWithoutDeps
        
buildAggregate :: Map.Map String [String] -> [String] -> Map.Map String [String]
buildAggregate agg rule =
    let key = last rule in
    let cur = Map.lookup key agg in
        case cur of Just x -> Map.insert key ((head rule) : x) agg
                    Nothing -> Map.insert key [(head rule)] agg

mkUniq :: Ord a => [a] -> [a]
mkUniq = Set.toList . Set.fromList

allSteps :: [[String]] -> [String]
allSteps pairs =
    mkUniq $ (map head pairs) ++ (map last pairs)

updateDeps :: Map.Map String [String] -> String -> Map.Map String [String]
updateDeps deps step =
    let depsList = Map.toList deps in
        let filteredDeps = map (\x -> (fst x, filter (\y -> y /= step) (snd x))) depsList in
            Map.delete step (Map.fromList filteredDeps)

getNextStep :: Map.Map String [String] -> (String, Map.Map String [String])
getNextStep deps =
    let availableSteps = map fst (filter (\x -> snd x == []) (Map.toList deps)) in
        let sortedAvailableSteps = List.sort availableSteps in
            let nextStep = List.head availableSteps in
                let updatedDeps = updateDeps deps nextStep in
                    (nextStep, updatedDeps)

-- assemble :: Map.Map String [String] -> String -> (Map.Map String [String], String)
-- assemble deps "" = 
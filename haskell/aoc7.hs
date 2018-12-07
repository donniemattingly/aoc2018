import System.IO  
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set

testFileName = "../inputs/input-7.0.txt"
fileName = "../inputs/input-7.1.txt"

main = do  
    handle <- openFile fileName ReadMode  
    contents <- hGetContents handle  
    let vals = map condenseDependency (readlines contents)
    let aggDeps = aggregateDependencies vals
    let result = partOne aggDeps

    print $ result
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

assemble :: Map.Map String [String] -> String -> String
assemble deps steps = 
    if deps == Map.empty
        then steps
    else 
        let (nextStep, updatedDeps) = getNextStep deps in
            assemble updatedDeps steps ++ nextStep

partOne :: Map.Map String [String] -> String
partOne deps =
    reverse (assemble deps "")

walkTimeLine :: (WorkerMap, DependencyMap, Int) -> Int

assembleWithHelp :: (WorkerMap, DependencyMap, String, Int) -> (String, Int)
assembleWithHelp (workers, deps, steps, time) = 
    if deps == Map.empty
        then (steps, time)
    else 
        let (nextStep, updatedDeps) = getNextStep deps in
            let (steps, time) = assembleWithHelp (workers, updatedDeps, steps ++ nextStep, 0) in
                    (steps, time)


type WorkerMap = Map.Map String (Maybe String, Maybe Int)
type DependencyMap = Map.Map String [String]

generateWorkerMap :: Int -> WorkerMap
generateWorkerMap size =
    -- Helpers + Individual
    let r = [0..size] in
        let emptyWorkers = foldl (\acc x -> Map.insert (show x) (Nothing, Nothing) acc) Map.empty r in
            emptyWorkers

getAvailableWorker :: WorkerMap -> Maybe String
getAvailableWorker workers = 
    (List.find (\x -> (fst (snd x)) == Nothing) (Map.toList workers)) >>=
        (\worker -> Just (fst (worker)))

getTimeFromWorker :: (String, (Maybe String, Maybe Int)) -> Int
getTimeFromWorker worker =
    case snd (snd worker) of 
        Just n -> n
        Nothing -> maxBound :: Int

getStepFromWorker :: (String, (Maybe String, Maybe Int)) -> String
getStepFromWorker worker =
    case fst (snd worker) of
        Just s -> s
        Nothing -> ""

updateDepsAndWorkers :: (WorkerMap, DependencyMap, String) -> (WorkerMap, DependencyMap, Int)
updateDepsAndWorkers (workers, deps, step) =
    let workingWorkers = filter (\(name, (curStep, time)) -> curStep /= Nothing) (Map.toList workers) in
        let nextWorkerToFinish = List.head (List.sortOn getTimeFromWorker workingWorkers) in
            let updatedWorkers = Map.insert (fst nextWorkerToFinish) (Nothing, Nothing) workers in
                let updatedDeps = updateDeps deps (getStepFromWorker nextWorkerToFinish) in
                    (updatedWorkers, updatedDeps, getTimeFromWorker nextWorkerToFinish)

-- very similar to getNextStep execept need to call new version of updateDeps
-- that takes into account the worker map and remaining time
getNextAssistedStep :: WorkerMap -> DependencyMap -> (WorkerMap, DependencyMap, Int, String)
getNextAssistedStep workers deps =
    let iden = (workers, deps, "") in
        let availableSteps = map fst (filter (\x -> snd x == []) (Map.toList deps)) in
            let sortedAvailableSteps = List.sort availableSteps in
                let openWorker = getAvailableWorker workers in
                    let nextStep = List.head availableSteps in
                            let (updatedWorkers, updatedDeps, elapsedTime) = updateDepsAndWorkers (workers, deps, nextStep) in
                                (updatedWorkers, updatedDeps, elapsedTime, nextStep)



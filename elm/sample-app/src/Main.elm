module Main exposing (..)

import Html
import Array exposing (Array, fromList)
import String
import List
import Debug


printableArray: Array Int -> String
printableArray array=
    let stringList = List.map (\x -> String.fromInt x) (Array.toList array) in
    String.join "" stringList


numToDigits : Int -> Array Int
numToDigits number = 
    String.split "" (String.fromInt number)
    |> List.map (\x -> 
        case String.toInt x of 
            Just a -> a
            Nothing -> 0
            )
    |> Array.fromList
                 

nextRecipe : Int -> Int -> Array Int -> (Int, Int, Array Int)
nextRecipe elf1 elf2 currentRecipes = 
    let elf1Score = case (Array.get elf1 currentRecipes) of
                        Just a -> a
                        Nothing -> 0
        elf2Score = case (Array.get elf2 currentRecipes) of
                        Just a -> a
                        Nothing -> 0
    in
        -- let 
            -- _ = Debug.log "elf1 score" elf1Score
            -- _ = Debug.log "elf2 score" elf2Score 
        -- in
        let recipes = Array.append currentRecipes (numToDigits (elf1Score + elf2Score))   
        in
            let numRecipes = Array.length recipes 
                -- _ = Debug.log "recipeCount" numRecipes 
            in                
               (modBy numRecipes (elf1 + elf1Score + 1) , modBy numRecipes (elf2 + elf2Score + 1), recipes)


getRecipes : Int -> Int -> Int -> Int -> Array Int -> Array Int
getRecipes curCount neededCount elf1 elf2 recipes =
    -- let _ = Debug.log "recipes: " (printableArray recipes)  in
    -- let _ = Debug.log "elf1: " elf1 in
    -- let _ = Debug.log "elf2: " elf2 in 

    if curCount >= neededCount then
        recipes
    else
        let (new1, new2, newRecipes) = nextRecipe elf1 elf2 recipes 
            recipeCount = Array.length newRecipes
        in
            getRecipes (Array.length newRecipes) neededCount new1 new2 newRecipes

initGetRecipes : Int -> Array Int
initGetRecipes count =
    let 
        initialRecipes = Array.fromList [3, 7] 
        elf1 = 0
        elf2 = 1
    in
        getRecipes 2 count elf1 elf2 initialRecipes


testPattern : String -> Array Int -> Bool
testPattern pattern array =
    let len = String.length pattern
        arrayLen = Array.length array
        testStr = printableArray(Array.slice (-1 * len) arrayLen array)
    in
        pattern == testStr


getRecipes2 : Int -> String -> Int -> Int -> Array Int -> Array Int
getRecipes2 curCount endPattern elf1 elf2 recipes =
    -- let _ = Debug.log "recipes: " (printableArray recipes)  in
    -- let _ = Debug.log "elf1: " elf1 in
    -- let _ = Debug.log "elf2: " elf2 in 

    let _ = 
            if (modBy 1000 curCount) == 0 then
                Debug.log "count" curCount
            else
                curCount
    in

    if testPattern endPattern recipes then
        recipes
    else
        let (new1, new2, newRecipes) = nextRecipe elf1 elf2 recipes 
            recipeCount = Array.length newRecipes
        in
            getRecipes2 (Array.length newRecipes) endPattern new1 new2 newRecipes

initGetRecipes2 : String -> Array Int
initGetRecipes2 pattern =
    let 
        initialRecipes = Array.fromList [3, 7] 
        elf1 = 0
        elf2 = 1
    in
        getRecipes2 2 pattern elf1 elf2 initialRecipes


partOne : Int -> String
partOne countTo = 
    let 
        actual = countTo + 10
        recipes = initGetRecipes actual
        size = Array.length recipes
        lastSlice = Array.slice countTo (countTo + 10) recipes
    in
        printableArray lastSlice
    
partTwo : String -> Int
partTwo pattern = 
    let recipes = initGetRecipes2 pattern
        leftSize = (Array.length recipes) - (String.length pattern)
    in
        leftSize
   
main = 
    let 
        result = partOne 10000000
        _ = Debug.log "result" result
    in
    Html.text "Hello World"

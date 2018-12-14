module Main exposing (..)

import Html
import Array exposing (Array, fromList)
import String
import List
import Debug


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
        let recipes = Array.append currentRecipes (numToDigits (elf1Score + elf2Score))   
        in
            let numRecipes = Array.length recipes 
            in                
               (modBy numRecipes (elf1Score + 1) , modBy numRecipes (elf2Score + 1), recipes)


getRecipes : Int -> Int -> Int -> Array Int -> Array Int
getRecipes curCount elf1 elf2 recipes =
    case curCount of
        0 -> recipes
        x -> let (new1, new2, newRecipes) = nextRecipe elf1 elf2 recipes 
             in
                getRecipes (curCount - 1) new1 new2 newRecipes

initGetRecipes : Int -> Array Int
initGetRecipes count =
    let 
        initialRecipes = Array.fromList [3, 7] 
        elf1 = 0
        elf2 = 1
    in
        getRecipes count elf1 elf2 initialRecipes

        
    

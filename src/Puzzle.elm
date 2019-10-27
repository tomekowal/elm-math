module Puzzle exposing (Puzzle, initial, isCorrect, puzzleGenerator, puzzleToString)

import Random


type Puzzle
    = Multiplication Int Int
    | Division Int Int
    | Addition Int Int
    | Subtraction Int Int


initial : Puzzle
initial =
    Multiplication 0 0


zeroToTen : Random.Generator Int
zeroToTen =
    Random.int 0 10


oneToTen : Random.Generator Int
oneToTen =
    Random.int 1 10


multGenerator : Random.Generator Puzzle
multGenerator =
    Random.map2 Multiplication zeroToTen zeroToTen


divGenerator : Random.Generator Puzzle
divGenerator =
    Random.map2 Division oneToTen zeroToTen


addGenerator : Random.Generator Puzzle
addGenerator =
    Random.map2 Addition zeroToTen zeroToTen


subGenerator : Random.Generator Puzzle
subGenerator =
    Random.map2 Subtraction zeroToTen zeroToTen


puzzleGenerator : Random.Generator Puzzle
puzzleGenerator =
    Random.int 1 4
        |> Random.andThen intToPuzzle


intToPuzzle : Int -> Random.Generator Puzzle
intToPuzzle int =
    case int of
        1 ->
            multGenerator

        2 ->
            divGenerator

        3 ->
            addGenerator

        _ ->
            subGenerator


isCorrect : Puzzle -> Int -> Bool
isCorrect puzzle guess =
    case puzzle of
        Multiplication left right ->
            left * right == guess

        Division left right ->
            right == guess

        Addition left right ->
            left + right == guess

        Subtraction left right ->
            right == guess


puzzleToString : Puzzle -> String
puzzleToString puzzle =
    case puzzle of
        Multiplication left right ->
            String.fromInt left ++ " x " ++ String.fromInt right

        Division left right ->
            String.fromInt (left * right) ++ " / " ++ String.fromInt left

        Addition left right ->
            String.fromInt left ++ " + " ++ String.fromInt right

        Subtraction left right ->
            String.fromInt (left + right) ++ " - " ++ String.fromInt left

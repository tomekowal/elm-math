module Puzzle exposing (Puzzle, initial, isCorrect, puzzleGenerator, puzzleToString)

import Random


type Puzzle
    = Multiplication Int Int
    | Addition Int Int


initial : Puzzle
initial =
    Multiplication 0 0


digitGenerator : Random.Generator Int
digitGenerator =
    Random.int 0 10


multGenerator : Random.Generator Puzzle
multGenerator =
    Random.map2 Multiplication digitGenerator digitGenerator


addGenerator : Random.Generator Puzzle
addGenerator =
    Random.map2 Addition digitGenerator digitGenerator


puzzleGenerator : Random.Generator Puzzle
puzzleGenerator =
    Random.int 1 2
        |> Random.andThen intToPuzzle


intToPuzzle : Int -> Random.Generator Puzzle
intToPuzzle int =
    case int of
        1 ->
            multGenerator

        _ ->
            addGenerator


isCorrect : Puzzle -> Int -> Bool
isCorrect puzzle guess =
    case puzzle of
        Multiplication left right ->
            left * right == guess

        Addition left right ->
            left + right == guess


puzzleToString : Puzzle -> String
puzzleToString puzzle =
    case puzzle of
        Multiplication left right ->
            String.fromInt left ++ " x " ++ String.fromInt right

        Addition left right ->
            String.fromInt left ++ " + " ++ String.fromInt right

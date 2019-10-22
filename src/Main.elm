module Main exposing (main)

import Browser
import Html exposing (Html, div, text, input, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Time
import Random

-- MAIN

main =
  Browser.element { init = init
                  , update = update
                  , view = view
                  , subscriptions = subscriptions
                  }
-- MODEL

type alias Guess = Maybe Int
type alias Time = Int
type alias Puzzle = (Int, Int)
type Game = Running Guess Time Puzzle
          | Stopped Puzzle

init : () -> (Game, Cmd Msg)
init _ = (Running Nothing 20 (0, 0), Random.generate NewPuzzle puzzleGenerator)

-- UPDATE

type Msg = Guess String
         | Restart
         | Tick Time.Posix
         | NewPuzzle Puzzle

update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
  case msg of
    Guess string ->
      case game of
        Running _ time puzzle ->
          if isCorrect puzzle (String.toInt string) then
            (Running Nothing (time + 1) puzzle, Random.generate NewPuzzle puzzleGenerator)
          else
            (Running (String.toInt string) time puzzle, Cmd.none)
        Stopped puzzle ->
          (Stopped puzzle, Cmd.none)
    Restart ->
      case game of
        Running _ _ _ ->
          (game, Cmd.none)
        Stopped _ ->
          init ()
    Tick _ ->
      case game of
        Running guess 0 puzzle ->
          (Stopped puzzle, Cmd.none)
        Running guess time puzzle ->
          (Running guess (time - 1) puzzle, Cmd.none)
        Stopped guess ->
          (Stopped guess, Cmd.none)
    NewPuzzle puzzle ->
      case game of
        Running guess time _ ->
          (Running guess time puzzle, Cmd.none)
        Stopped _ ->
          (Stopped puzzle, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Game -> Sub Msg
subscriptions game =
  case game of
    Running _ _ _ ->
      Time.every 1000 Tick
    Stopped _ ->
      Sub.none

-- VIEW

view : Game -> Html Msg
view game =
  div []
    (
      [ timer game
      , div [] [ text "Score: 0" ]
      , div [] [ text (puzzleToString game) ]
      , input [ value (guessToString game), onInput Guess ] []
      ] ++ maybeTryAgainButton game
    )

timer : Game -> Html Msg
timer game =
  case game of
    Running guess time _ ->
      div [ style "width" (timerWidth time)
          , style "background-color" "red"
          , style "height" "20px"
          ] []
    Stopped _ ->
      div [] [ text "Game Over!" ]

puzzleToString : Game -> String
puzzleToString game =
  case game of
    Running _ _ (left, right) ->
      (String.fromInt left) ++ " x " ++ (String.fromInt right)
    Stopped (left, right) ->
      (String.fromInt left) ++ " x " ++ (String.fromInt right)

timerWidth : Int -> String
timerWidth x =
  String.concat [ (String.fromInt (x * 20)), "px" ]

maybeTryAgainButton : Game -> List (Html Msg)
maybeTryAgainButton game =
  case game of
    Running _ _ _ ->
      []
    Stopped _ ->
      [ button [ onClick Restart ] [ text "Try again!" ] ]

guessToString : Game -> String
guessToString game =
  case game of
    Running (Just int) _ _ ->
      String.fromInt int
    Running Nothing _ _ ->
      ""
    Stopped _ ->
      ""

isCorrect : Puzzle -> Guess -> Bool
isCorrect puzzle guess =
  case guess of
    Just guessValue ->
      case puzzle of
        (left, right) ->
          left * right == guessValue
    Nothing ->
      False

factorGenerator : Random.Generator Int
factorGenerator = Random.int 0 10

puzzleGenerator : Random.Generator Puzzle
puzzleGenerator = Random.pair factorGenerator factorGenerator

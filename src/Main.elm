module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Random
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Guess =
    Maybe Int


type alias Time =
    Int


type alias Puzzle =
    ( Int, Int )


type alias Game =
    { guess : Guess
    , time : Time
    , puzzle : Puzzle
    , score : Int
    }


init : () -> ( Game, Cmd Msg )
init _ =
    let
        game =
            Game Nothing 20 ( 0, 0 ) 0

        generator =
            Random.generate NewPuzzle puzzleGenerator
    in
    ( game, generator )



-- UPDATE


type Msg
    = Guess String
    | Restart
    | Tick Time.Posix
    | NewPuzzle Puzzle


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case game.time of
        0 ->
            case msg of
                Tick _ ->
                    ( game, Cmd.none )

                NewPuzzle _ ->
                    ( game, Cmd.none )

                Guess _ ->
                    ( game, Cmd.none )

                Restart ->
                    init ()

        time ->
            case msg of
                Guess stringGuess ->
                    let
                        guess =
                            String.toInt stringGuess
                    in
                    if isCorrect game.puzzle guess then
                        ( { game | time = game.time + 1, guess = Nothing }
                        , Random.generate NewPuzzle puzzleGenerator
                        )

                    else
                        ( { game | guess = guess }, Cmd.none )

                Restart ->
                    ( game, Cmd.none )

                Tick _ ->
                    ( { game | time = time - 1 }, Cmd.none )

                NewPuzzle puzzle ->
                    ( { game | puzzle = puzzle }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Game -> Sub Msg
subscriptions game =
    case game.time of
        0 ->
            Sub.none

        time ->
            Time.every 1000 Tick



-- VIEW


view : Game -> Html Msg
view game =
    div []
        ([ timer game
         , div [] [ text "Score: 0" ]
         , div [] [ text (puzzleToString game.puzzle) ]
         , input [ value (guessToString game.guess), onInput Guess ] []
         ]
            ++ maybeTryAgainButton game
        )


timer : Game -> Html Msg
timer game =
    case game.time of
        0 ->
            div [] [ text "Game Over!" ]

        time ->
            div
                [ style "width" (timerWidth time)
                , style "background-color" "red"
                , style "height" "20px"
                ]
                []


puzzleToString : Puzzle -> String
puzzleToString ( left, right ) =
    String.fromInt left ++ " x " ++ String.fromInt right


timerWidth : Int -> String
timerWidth x =
    String.concat [ String.fromInt (x * 20), "px" ]


maybeTryAgainButton : Game -> List (Html Msg)
maybeTryAgainButton game =
    case game.time of
        0 ->
            [ button [ onClick Restart ] [ text "Try again!" ] ]

        time ->
            []


guessToString : Guess -> String
guessToString guess =
    case guess of
        Just int ->
            String.fromInt int

        Nothing ->
            ""


isCorrect : Puzzle -> Guess -> Bool
isCorrect puzzle guess =
    case guess of
        Just guessValue ->
            case puzzle of
                ( left, right ) ->
                    left * right == guessValue

        Nothing ->
            False


factorGenerator : Random.Generator Int
factorGenerator =
    Random.int 0 10


puzzleGenerator : Random.Generator Puzzle
puzzleGenerator =
    Random.pair factorGenerator factorGenerator

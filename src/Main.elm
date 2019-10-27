module Main exposing (main)

import Browser
import Element exposing (Element, centerX, centerY, column, el, height, htmlAttribute, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, button, div)
import Html.Attributes exposing (attribute)
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


type alias Settings =
    { addition : Bool }


type alias Game =
    { guess : Guess
    , time : Time
    , puzzle : Puzzle
    , score : Int
    , settings : Settings
    }


init : () -> ( Game, Cmd Msg )
init _ =
    let
        game =
            Game Nothing 20 ( 0, 0 ) 0 initialSettings

        generator =
            Random.generate NewPuzzle puzzleGenerator
    in
    ( game, generator )


initialSettings : Settings
initialSettings =
    { addition = True }



-- UPDATE


type SettingMsg
    = Addition Bool


type Msg
    = Guess String
    | Restart
    | Tick Time.Posix
    | NewPuzzle Puzzle
    | Setting SettingMsg


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

                Setting settingMsg ->
                    updateGameSettings settingMsg game

        time ->
            case msg of
                Guess stringGuess ->
                    let
                        guess =
                            String.toInt stringGuess
                    in
                    if isCorrect game.puzzle guess then
                        ( { game | time = game.time + 1, guess = Nothing, score = game.score + 1 }
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

                Setting settingMsg ->
                    updateGameSettings settingMsg game


updateGameSettings : SettingMsg -> Game -> ( Game, Cmd Msg )
updateGameSettings settingMsg game =
    let
        newSettings =
            updateSettings settingMsg game.settings
    in
    ( { game | settings = newSettings }, Cmd.none )


updateSettings : SettingMsg -> Settings -> Settings
updateSettings settingMsg settings =
    case settingMsg of
        Addition bool ->
            { settings | addition = bool }



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
    Element.layout [] (container game)


container : Game -> Element Msg
container game =
    column [ spacing 10, centerX ]
        [ timer game
        , el defaultStyle (Element.text ("Score: " ++ String.fromInt game.score))
        , el defaultStyle (Element.text (puzzleToString game.puzzle))
        , Input.text (Input.focusedOnLoad :: htmlAttribute numericKeyboardAttr :: defaultStyle)
            { text = guessToString game.guess
            , onChange = Guess
            , placeholder = Nothing
            , label = Input.labelHidden "Guess"
            }
        , maybeTryAgainButton game
        ]



-- I want the same height of Timer Bar as Game Over text,
-- so I insert space in empty timer


timer : Game -> Element Msg
timer game =
    case game.time of
        0 ->
            el defaultStyle (Element.text "Game Over!")

        time ->
            el
                ([ width (px (timerWidth time))
                 , Background.color primaryColor
                 ]
                    ++ defaultStyle
                )
                (Element.text " ")


puzzleToString : Puzzle -> String
puzzleToString ( left, right ) =
    String.fromInt left ++ " x " ++ String.fromInt right


timerWidth : Int -> Int
timerWidth x =
    x * 20


maybeTryAgainButton : Game -> Element Msg
maybeTryAgainButton game =
    case game.time of
        0 ->
            Input.button defaultStyle
                { label = Element.text "Try again!"
                , onPress = Just Restart
                }

        time ->
            el defaultStyle (Element.text " ")


settingsRow : Game -> Element Msg
settingsRow game =
    row defaultStyle
        [ Input.checkbox []
            { onChange = \bool -> Setting (Addition bool)
            , icon = Input.defaultCheckbox
            , checked = game.settings.addition
            , label =
                Input.labelBelow []
                    (text "+")
            }
        ]


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


defaultStyle =
    [ centerX, centerY, Font.size 50 ]


primaryColor =
    rgb255 240 154 154


numericKeyboardAttr =
    attribute "pattern" "\\d*"

module Main exposing (main)

import Browser
import Browser.Dom
import Element exposing (Element, centerX, centerY, column, el, height, htmlAttribute, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, button, div)
import Html.Attributes exposing (attribute)
import Html.Events exposing (onClick, onInput)
import Puzzle
import Random
import Task
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


type alias Model =
    { guess : Guess
    , time : Time
    , puzzle : Puzzle.Puzzle
    , score : Int
    , settings : Settings
    }


type alias Guess =
    Maybe Int


type alias Time =
    Int


type alias Settings =
    { addition : Bool }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            Model Nothing 20 Puzzle.initial 0 initialSettings

        generator =
            Random.generate NewPuzzle Puzzle.puzzleGenerator

        focusEvent =
            Task.attempt (\_ -> NoOp) (Browser.Dom.focus "guess")
    in
    ( model, Cmd.batch [ generator, focusEvent ] )


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
    | NewPuzzle Puzzle.Puzzle
    | Setting SettingMsg
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            case model.time of
                0 ->
                    ( model, Cmd.none )

                time ->
                    ( decreaseTime model, Cmd.none )

        NewPuzzle puzzle ->
            case model.time of
                0 ->
                    ( model, Cmd.none )

                _ ->
                    ( model |> withPuzzle puzzle, Cmd.none )

        Guess stringGuess ->
            handleGuess stringGuess model

        Restart ->
            case model.time of
                0 ->
                    init ()

                _ ->
                    ( model, Cmd.none )

        Setting settingMsg ->
            ( updateModelSettings settingMsg model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


decreaseTime : Model -> Model
decreaseTime model =
    { model | time = model.time - 1 }


increaseTime : Model -> Model
increaseTime model =
    { model | time = model.time + 1 }


clearGuess : Model -> Model
clearGuess model =
    { model | guess = Nothing }


withGuess : Guess -> Model -> Model
withGuess guess model =
    { model | guess = guess }


withPuzzle : Puzzle.Puzzle -> Model -> Model
withPuzzle puzzle model =
    { model | puzzle = puzzle }


increaseScore : Model -> Model
increaseScore model =
    { model | score = model.score + 1 }


updateModelSettings : SettingMsg -> Model -> Model
updateModelSettings settingMsg model =
    let
        newSettings =
            updateSettings settingMsg model.settings
    in
    { model | settings = newSettings }


updateSettings : SettingMsg -> Settings -> Settings
updateSettings settingMsg settings =
    case settingMsg of
        Addition bool ->
            { settings | addition = bool }


handleGuess : String -> Model -> ( Model, Cmd Msg )
handleGuess stringGuess model =
    case model.time of
        0 ->
            ( model, Cmd.none )

        _ ->
            let
                guess =
                    String.toInt stringGuess
            in
            if isCorrect model.puzzle guess then
                ( model
                    |> increaseScore
                    |> increaseTime
                    |> clearGuess
                , Random.generate NewPuzzle Puzzle.puzzleGenerator
                )

            else
                ( model |> withGuess guess, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.time of
        0 ->
            Sub.none

        time ->
            Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [] (container model)


container : Model -> Element Msg
container model =
    column [ spacing 10, centerX ]
        [ timer model.time
        , el defaultStyle <| Element.text <| "Score: " ++ String.fromInt model.score
        , el defaultStyle <| Element.text <| Puzzle.puzzleToString model.puzzle
        , Input.text (Input.focusedOnLoad :: htmlAttribute numericKeyboardAttr :: htmlAttribute inputId :: defaultStyle)
            { text = guessToString model.guess
            , onChange = Guess
            , placeholder = Nothing
            , label = Input.labelHidden "Guess"
            }
        , maybeTryAgainButton model.time
        ]



-- I want the same height of Timer Bar as Model Over text,
-- so I insert space in empty timer


timer : Time -> Element Msg
timer time =
    case time of
        0 ->
            el defaultStyle <| Element.text "Game Over!"

        _ ->
            el
                ([ width <| px <| timerWidth time
                 , Background.color primaryColor
                 ]
                    ++ defaultStyle
                )
                (Element.text " ")


timerWidth : Int -> Int
timerWidth x =
    x * 20


maybeTryAgainButton : Time -> Element Msg
maybeTryAgainButton time =
    case time of
        0 ->
            Input.button defaultStyle
                { label = Element.text "Try again!"
                , onPress = Just Restart
                }

        _ ->
            el defaultStyle <| Element.text " "


settingsRow : Settings -> Element Msg
settingsRow settings =
    row defaultStyle
        [ Input.checkbox []
            { onChange = \bool -> Setting <| Addition bool
            , icon = Input.defaultCheckbox
            , checked = settings.addition
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


isCorrect : Puzzle.Puzzle -> Guess -> Bool
isCorrect puzzle guess =
    case guess of
        Just guessValue ->
            Puzzle.isCorrect puzzle guessValue

        Nothing ->
            False


defaultStyle =
    [ centerX, centerY, Font.size 50 ]


primaryColor =
    rgb255 240 154 154


numericKeyboardAttr =
    attribute "pattern" "\\d*"


inputId =
    attribute "id" "guess"

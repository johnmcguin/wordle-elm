module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Keyboard



-- Main


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- Model
{-
   nyt note - store list of guesses. store current row index
   Keyboard:
   - needs to render a keyboard. But also needs state. Needs to track which letters in the
   keyboard have been guessed as well as the type of guess (Unguessed, CorrectGuess, IncorrectGuessInWord, IncorrectGuess)
   - needs to work on touch events (button) as well as keydown events (typing)

   Game Board:
   - needs to render a 6x5 board (6 guesses of 5 letter words)
   - needs to track keypresses from the keyboard and render the letters pressed in real time for the _current_ row
   - needs to track which row is currently being guessed
   - needs to track the state of the letters against the puzzle word for a given guess (guessed?, incorrect in word, correct in word, not in word)
   - needs to check if a submitted guess is in the list of viable words
   - needs to check if a submitted guess is correct
   - needs to track final row / guess / finish of the game
   - needs to handle end game (show result)
   - needs to track list of guesses (to hydrate the game board if refresh, etc)

   Shared state:
   - letters guessed and their state
   - game (Initial, Playing, Over)
    - if Initial, game board is empty, keyboard is empty
    - if Playing, game board is _possibly_ filles, keyboard is _possibly_ filled
    - if Over, game board is filled, keyboard is filled
-}


type Model
    = Initial
    | Playing Keyboard.Model
    | Ended


init : () -> ( Model, Cmd Msg )
init _ =
    ( Initial, Cmd.none )



-- Update
-- List of events in the game:
-- - KeyPressed String (is this is?)


type Msg
    = KeyboardMsg Keyboard.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Initial, KeyboardMsg keyboardMsg ) ->
            let
                ( keyboardModel, keyboardCmd ) =
                    Keyboard.init 1
            in
            ( Playing keyboardModel, Cmd.none )

        ( Playing keyboardState, KeyboardMsg keyboardMsg ) ->
            let
                ( keyboardModel, keyboardCmd ) =
                    Keyboard.update keyboardMsg keyboardState
            in
            ( Playing keyboardModel, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Views
-- \u{23ce} - enter key
-- \u{232b} - delete key


keyboardRows : List (List Char)
keyboardRows =
    [ [ 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p' ]
    , [ 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l' ]
    , [ '⏎', 'z', 'x', 'c', 'v', 'b', 'n', 'm', '⌫' ]
    ]


view : Model -> Html Msg
view _ =
    div [ HA.class "app" ]
        [ div [ HA.class "board_wrapper" ]
            [ div [ HA.class "board" ]
                [ div [ HA.class "board_row" ]
                    [ div [ HA.class "tile" ] [ text "t" ]
                    , div [ HA.class "tile" ] [ text "h" ]
                    , div [ HA.class "tile" ] [ text "e" ]
                    , div [ HA.class "tile" ] [ text "i" ]
                    , div [ HA.class "tile" ] [ text "r" ]
                    ]
                ]
            ]
        , div [ HA.class "keyboard" ] (List.map renderRow keyboardRows)
        , button [ onClick (KeyboardMsg Keyboard.Increment) ] [ text "click me" ]
        ]


renderRow : List Char -> Html Msg
renderRow chars =
    div [ HA.class "keyboard_row" ] (List.map (\char -> button [ HA.class "key" ] [ text (String.fromChar char) ]) chars)

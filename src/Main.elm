module Main exposing (main)

import Browser
import Game
import Html exposing (Html, div, text)
import Html.Attributes as HA



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


type GameResult
    = WonIn Int -- count
    | Lost


type alias GameBoard =
    { currentRow : Int
    }


type Model
    = Initial
    | Playing { game : Game.Model }
    | Ended GameResult


init : () -> ( Model, Cmd Msg )
init _ =
    ( Initial, Cmd.none )



-- Update
-- List of events in the game:
-- - KeyPressed String (is this is?)


type Msg
    = ToGame Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Initial, ToGame gameMsg ) ->
            let
                ( gameModel, _ ) =
                    Game.init |> Game.update gameMsg
            in
            ( Playing { game = gameModel }, Cmd.none )

        ( Playing gameState, ToGame gameMsg ) ->
            let
                ( gameModel, _ ) =
                    Game.update gameMsg gameState.game
            in
            -- this pattern ensures that commands produce keybard messages instead of Main messages
            -- Leverages internal ToGame constructor which delegates to Keyboard module
            -- ( Playing keyboardModel, Cmd.map ToGame keyboardCmd )
            ( Playing { gameState | game = gameModel }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Views


view : Model -> Html Msg
view model =
    case model of
        Initial ->
            Game.init |> layout

        Playing state ->
            layout state.game

        Ended _ ->
            div [] [ text "handle end game" ]


layout : Game.Model -> Html Msg
layout gameModel =
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

        -- this maps Messages from Keyboard view into this module's "ToGame" msg
        , gameModel |> Game.view |> Html.map ToGame
        ]

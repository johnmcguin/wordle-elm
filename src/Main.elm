module Main exposing (main)

import Browser
import Browser.Events
import Game
import Html exposing (Html, div, text)
import Json.Decode as D



-- Main


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- Model


type GameResult
    = WonIn Int
    | Lost


type Model
    = Initial
    | Playing { game : Game.Model }
    | Ended GameResult


init : () -> ( Model, Cmd Msg )
init _ =
    ( Playing { game = Game.init }, Cmd.none )



-- Update


type Msg
    = ToGame Game.Msg
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Initial, ToGame gameMsg ) ->
            let
                ( gameModel, _ ) =
                    Game.init |> Game.update gameMsg
            in
            ( Playing { game = gameModel }, Cmd.none )

        ( Playing state, ToGame gameMsg ) ->
            let
                ( gameModel, _ ) =
                    Game.update gameMsg state.game
            in
            -- this pattern ensures that commands produce keybard messages instead of Main messages
            -- Leverages internal ToGame constructor which delegates to Keyboard module
            -- ( Playing keyboardModel, Cmd.map ToGame keyboardCmd )
            ( Playing { state | game = gameModel }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyUp keyDecoder


keyDecoder : D.Decoder Msg
keyDecoder =
    D.map toKey (D.field "key" D.string)


toKey : String -> Msg
toKey key =
    case String.uncons key of
        Just ( char, "" ) ->
            let
                normalizedChar =
                    Char.toLower char

                code =
                    Char.toCode normalizedChar

                a =
                    97

                z =
                    122
            in
            if code >= a && code <= z then
                ToGame (Game.KeyPress normalizedChar)

            else
                NoOp

        _ ->
            case key of
                "Backspace" ->
                    ToGame Game.Delete

                "Enter" ->
                    ToGame Game.SubmitGuess

                _ ->
                    NoOp



-- Views


view : Model -> Html Msg
view model =
    case model of
        Initial ->
            Game.init |> Game.view |> Html.map ToGame

        Playing state ->
            Game.view state.game |> Html.map ToGame

        Ended _ ->
            div [] [ text "handle end game" ]

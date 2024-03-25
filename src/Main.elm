module Main exposing (main)

import Browser
import Browser.Events
import Game
import Html exposing (Html)
import Json.Decode as D



-- Main


type alias Flags =
    { word : String
    }


main : Program D.Value Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- Model


type Model
    = Game Game.Model


init : D.Value -> ( Model, Cmd Msg )
init flags =
    case D.decodeValue flagsDecoder flags of
        Ok result ->
            ( Game <| Game.init result.word, Cmd.none )

        Err _ ->
            ( Game <| Game.init "", Cmd.map ToGame Game.getRandomWord )


flagsDecoder : D.Decoder Flags
flagsDecoder =
    D.map Flags (D.field "word" D.string)



-- Update


type Msg
    = ToGame Game.Msg
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Game gameState, ToGame gameMsg ) ->
            let
                ( gameModel, gameCmd ) =
                    Game.update gameMsg gameState
            in
            -- this pattern ensures that commands produce keybard messages instead of Main messages
            -- Leverages internal ToGame constructor which delegates to Keyboard module
            -- ( Playing keyboardModel, Cmd.map ToGame keyboardCmd )
            ( Game gameModel, Cmd.map ToGame gameCmd )

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
        Game gameState ->
            Game.view gameState |> Html.map ToGame

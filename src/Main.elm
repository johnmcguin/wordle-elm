module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes as HA



-- Main


main : Program () Int Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- Model
-- nyt note - store list of guesses. store current row index


type alias Model =
    Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( 0, Cmd.none )



-- Update


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Increment ->
            ( model + 1, Cmd.none )

        Decrement ->
            ( model - 1, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Views
-- \u{23ce} - enter key
-- \u{232b} - delete key


rows : List (List Char)
rows =
    [ [ 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p' ]
    , [ 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l' ]
    , [ '⏎', 'z', 'x', 'c', 'v', 'b', 'n', 'm', '⌫' ]
    ]


view : Model -> Html Msg
view _ =
    div [ HA.class "app" ]
        [ div [ HA.class "board_wrapper" ]
            [ div [ HA.class "board" ] [ text "todo board" ]
            ]
        , div [ HA.class "keyboard" ] (List.map renderRow rows)
        ]


renderRow : List Char -> Html Msg
renderRow chars =
    div [ HA.class "keyboard_row" ] (List.map (\char -> button [ HA.class "key" ] [ text (String.fromChar char) ]) chars)

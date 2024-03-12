module Game exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, text)
import Html.Attributes as HA
import Html.Events exposing (onClick)



-- \u{23ce} - enter key
-- \u{232b} - delete key
-- Model


type alias Model =
    { letters : List (List Char)
    , currentGuess : List Char
    , submitGuess : Bool
    }


init : Model
init =
    { letters =
        [ [ 'q'
          , 'w'
          , 'e'
          , 'r'
          , 't'
          , 'y'
          , 'u'
          , 'i'
          , 'o'
          , 'p'
          ]
        , [ 'a'
          , 's'
          , 'd'
          , 'f'
          , 'g'
          , 'h'
          , 'j'
          , 'k'
          , 'l'
          ]
        , [ '⏎'
          , 'z'
          , 'x'
          , 'c'
          , 'v'
          , 'b'
          , 'n'
          , 'm'
          , '⌫'
          ]
        ]
    , currentGuess = []
    , submitGuess = False
    }



-- Update


type Msg
    = KeyPress Char


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- \u{23ce} - enter key
        -- \u{232b} - delete key
        KeyPress '⌫' ->
            let
                currentGuess =
                    model.currentGuess
                        |> List.reverse
                        |> List.drop 1
                        |> List.reverse
            in
            ( { model | currentGuess = currentGuess }, Cmd.none )

        KeyPress '⏎' ->
            let
                submittable =
                    List.length model.currentGuess == 5
            in
            ( { model | submitGuess = submittable }, Cmd.none )

        KeyPress key ->
            if List.length model.currentGuess < 5 then
                ( { model | currentGuess = model.currentGuess ++ [ key ] }, Cmd.none )

            else
                ( model, Cmd.none )



-- Views


view : Model -> Html Msg
view model =
    div [ HA.class "keyboard" ] (List.map renderRow model.letters)


renderRow : List Char -> Html Msg
renderRow letter_rows =
    let
        key_rows =
            List.map renderBtn letter_rows
    in
    div [ HA.class "keyboard_row" ] key_rows


renderBtn : Char -> Html Msg
renderBtn letter =
    button [ HA.class "key", onClick (KeyPress letter) ] [ String.fromChar letter |> text ]

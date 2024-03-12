module Game exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, text)
import Html.Attributes as HA
import Html.Events exposing (onClick)


type Letter
    = Empty Char
    | Correct Char
    | Incorrect Char
    | Present Char



-- \u{23ce} - enter key
-- \u{232b} - delete key
-- Model


type alias Model =
    { letters : List (List Letter)
    , currentGuess : List Char
    , submitGuess : Bool
    }


init : Model
init =
    { letters =
        [ [ Empty 'q'
          , Empty 'w'
          , Empty 'e'
          , Empty 'r'
          , Empty 't'
          , Empty 'y'
          , Empty 'u'
          , Empty 'i'
          , Empty 'o'
          , Empty 'p'
          ]
        , [ Empty 'a'
          , Empty 's'
          , Empty 'd'
          , Empty 'f'
          , Empty 'g'
          , Empty 'h'
          , Empty 'j'
          , Empty 'k'
          , Empty 'l'
          ]
        , [ Empty '⏎'
          , Empty 'z'
          , Empty 'x'
          , Empty 'c'
          , Empty 'v'
          , Empty 'b'
          , Empty 'n'
          , Empty 'm'
          , Empty '⌫'
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


renderRow : List Letter -> Html Msg
renderRow letter_rows =
    let
        key_rows =
            List.map renderBtn letter_rows
    in
    div [ HA.class "keyboard_row" ] key_rows


renderBtn : Letter -> Html Msg
renderBtn letter =
    case letter of
        Empty char ->
            button [ HA.class "key", onClick (KeyPress char) ] [ String.fromChar char |> text ]

        Correct char ->
            button [ HA.class "key", onClick (KeyPress char) ] [ String.fromChar char |> text ]

        Incorrect char ->
            button [ HA.class "key", onClick (KeyPress char) ] [ String.fromChar char |> text ]

        Present char ->
            button [ HA.class "key", onClick (KeyPress char) ] [ String.fromChar char |> text ]

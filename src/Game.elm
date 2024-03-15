module Game exposing (Model, Msg(..), getRandomWord, init, update, view)

import Html exposing (Html, button, div, text)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import List.Extra as LE
import Random
import Words exposing (getRandom, wordLength)



-- \u{23ce} - enter key
-- \u{232b} - delete key
-- Model


type LetterState
    = Blank
    | Pending
    | Correct
    | Present
    | Incorrect


type alias Letter =
    ( Char, LetterState )


type GameResult
    = WonIn Int
    | Lost


type alias GameInProgess =
    { keyboardLetters : List (List Char)
    , currentGuess : List Char
    , currentRow : Int
    , solution : String
    , board : List (List Letter)
    }



-- type alias Model =
--     { keyboardLetters : List (List Char)
--     , currentGuess : List Char
--     , currentRow : Int
--     , solution : String
--     , board : List (List Letter)
--     }


type Model
    = InProgress GameInProgess
    | GameEnd GameResult


init : String -> Model
init solution =
    -- many state bits will need to be dynamic in the case of a refresh
    InProgress
        { keyboardLetters =
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
            , [ '0'
              , 'a'
              , 's'
              , 'd'
              , 'f'
              , 'g'
              , 'h'
              , 'j'
              , 'k'
              , 'l'
              , '0'
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
        , currentRow = 0
        , board = List.repeat 6 <| List.repeat 5 ( ' ', Blank )
        , solution = solution
        }



-- Update


type Msg
    = KeyPress Char
    | SubmitGuess
    | Delete
    | GotRandomIndex Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( InProgress gameState, KeyPress key ) ->
            let
                guessWritable =
                    List.length gameState.currentGuess < 5

                -- get the current row we are guessing for
                row =
                    gameState.board
                        |> LE.getAt gameState.currentRow
                        |> Maybe.withDefault []

                -- find the first blank cell (that is the target to fill for the next input value
                cell =
                    row
                        |> find (\( char, _ ) -> char == ' ')

                updatedRow =
                    case cell of
                        Just c ->
                            row
                                |> LE.updateAt (Tuple.first c) (\_ -> ( key, Pending ))

                        Nothing ->
                            row
            in
            if guessWritable then
                ( InProgress
                    { gameState
                        | currentGuess = gameState.currentGuess ++ [ key ]
                        , board =
                            gameState.board
                                |> List.indexedMap
                                    (\idx letterRow ->
                                        if idx == gameState.currentRow then
                                            updatedRow

                                        else
                                            letterRow
                                    )
                    }
                , Cmd.none
                )

            else
                -- shake?
                ( model, Cmd.none )

        ( InProgress gameState, SubmitGuess ) ->
            -- get board row at model.currentRow
            -- validate if the length is 5
            -- Join the characters to create a string version of the word
            -- Check to see if the guessed word is the model.solution
            -- If yes, WIN the game
            -- If not, compare the guessed word to the list of possible words
            --  -- if not in list, then stop the guess and show a message that the word is not supported
            --  -- if it is in the list, submit the guess and update the board row to the correct states (check each char position against the same position in the solution word and update its state)
            let
                isPending : Letter -> Bool
                isPending letter =
                    case letter of
                        ( _, Pending ) ->
                            True

                        _ ->
                            False

                isSubmittable =
                    gameState.board
                        |> LE.getAt gameState.currentRow
                        |> Maybe.map
                            (\row ->
                                List.all isPending row
                            )
                        |> Maybe.withDefault False

                guess =
                    gameState.board
                        |> LE.getAt gameState.currentRow
                        |> Maybe.map (\row -> List.map Tuple.first row)
                        |> Maybe.map (\letters -> String.fromList letters)
                        |> Maybe.withDefault ""

                gameWon =
                    guess == gameState.solution
            in
            -- handle end game, etc
            ( InProgress gameState, Cmd.none )

        ( InProgress gameState, Delete ) ->
            let
                row =
                    gameState.board
                        |> LE.getAt gameState.currentRow
                        |> Maybe.withDefault []

                backwards_row =
                    row |> List.reverse

                cell =
                    backwards_row
                        |> find (\( char, _ ) -> char /= ' ')

                updatedRow =
                    case cell of
                        Just c ->
                            backwards_row
                                |> LE.updateAt (Tuple.first c) (\_ -> ( ' ', Blank ))
                                |> List.reverse

                        Nothing ->
                            row

                currentGuess =
                    gameState.currentGuess
                        |> List.reverse
                        |> List.drop 1
                        |> List.reverse
            in
            ( InProgress
                { gameState
                    | currentGuess = currentGuess
                    , board =
                        gameState.board
                            |> List.indexedMap
                                (\idx letterRow ->
                                    if idx == gameState.currentRow then
                                        updatedRow

                                    else
                                        letterRow
                                )
                }
            , Cmd.none
            )

        ( InProgress gameState, GotRandomIndex idx ) ->
            ( InProgress { gameState | solution = getRandom idx |> Maybe.withDefault "" }
            , Cmd.none
            )

        _ ->
            Debug.todo "handle rest"



-- Views


view : Model -> Html Msg
view model =
    case model of
        InProgress gameState ->
            div [ HA.class "app" ]
                [ div [ HA.class "board_wrapper" ]
                    [ div [ HA.class "board" ] (List.map renderBoardRow gameState.board) ]
                , div [ HA.class "keyboard" ] (List.map renderRow gameState.keyboardLetters)
                ]

        _ ->
            div [] [ text "todo - handle other states" ]


renderBoardRow : List Letter -> Html Msg
renderBoardRow boardRow =
    div [ HA.class "board_row" ] (List.map renderBoardRowItems boardRow)


renderBoardRowItems : Letter -> Html Msg
renderBoardRowItems letter =
    div [ HA.class "tile" ] [ text (String.fromChar <| Tuple.first letter) ]


renderRow : List Char -> Html Msg
renderRow letter_rows =
    let
        key_rows =
            List.map renderBtn letter_rows
    in
    div [ HA.class "keyboard_row" ] key_rows


renderBtn : Char -> Html Msg
renderBtn letter =
    case letter of
        '⌫' ->
            button [ HA.class "key is_delete", onClick Delete ] [ String.fromChar letter |> text ]

        '⏎' ->
            button [ HA.class "key is_enter", onClick SubmitGuess ] [ String.fromChar letter |> text ]

        '0' ->
            div [ HA.class "key is_spacer" ] []

        char ->
            button [ HA.class (keyClass char), onClick <| KeyPress letter ] [ String.fromChar letter |> text ]


keyClass : Char -> String
keyClass letter =
    "key " ++ "is_" ++ String.fromChar letter



-- Utils


find : (a -> Bool) -> List a -> Maybe ( Int, a )
find pred list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if pred x then
                Just ( 0, x )

            else
                Maybe.map (\( index, item ) -> ( index + 1, item )) <| find pred xs


getRandomWord : Cmd Msg
getRandomWord =
    let
        generator : Random.Generator Int
        generator =
            Random.int 0 wordLength
    in
    Random.generate GotRandomIndex generator

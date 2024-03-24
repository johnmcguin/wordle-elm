module Game exposing (Model, Msg(..), getRandomWord, init, update, view)

import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import List.Extra as LE
import Process
import Random
import Task
import Words exposing (getRandom, wordIsValid, wordLength)



-- Model


type LetterState
    = Blank
    | Pending
    | Correct
    | Present
    | Incorrect


type alias Letter =
    ( Char, LetterState )


type alias KeyboardDictionary =
    Dict Char LetterState


type alias KeyboardRow =
    List Letter


type EndResult
    = WonIn Int
    | Lost


type alias GameResult =
    { result : EndResult
    , board : List KeyboardRow
    , keyboardLetters : List (List Char)
    , solution : String
    , message : Maybe String
    }


type alias GameInProgress =
    { keyboardLetters : List (List Char)
    , keyboardDictionary : KeyboardDictionary
    , currentGuess : List Char
    , currentRow : Int
    , solution : String
    , shakeRow : Maybe Int
    , board : List KeyboardRow
    , message : Maybe String
    }


type Model
    = InProgress GameInProgress
    | GameEnd GameResult


init : String -> Model
init solution =
    InProgress
        { keyboardLetters = initKeyboardDictLetters
        , keyboardDictionary = initKeyboardDict
        , currentGuess = []
        , currentRow = 0
        , board = initBoard
        , solution = solution
        , shakeRow = Nothing
        , message = Nothing
        }



-- Update


type Msg
    = KeyPress Char
    | SubmitGuess
    | Delete
    | GotRandomIndex Int
    | ClearAnimation
    | ClearAlert


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( InProgress gameState, KeyPress key ) ->
            let
                guessWritable =
                    List.length gameState.currentGuess < 5

                row =
                    gameState.board
                        |> LE.getAt gameState.currentRow
                        |> Maybe.withDefault []

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
                ( model, Cmd.none )

        ( InProgress gameState, SubmitGuess ) ->
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
                        |> Maybe.map (List.all isPending)
                        |> Maybe.withDefault False

                guess =
                    gameState.board
                        |> LE.getAt gameState.currentRow
                        |> Maybe.map (List.map Tuple.first)
                        |> Maybe.map String.fromList
                        |> Maybe.withDefault ""

                gameWon =
                    isSubmittable && guess == gameState.solution

                gameLost =
                    isSubmittable && guess /= gameState.solution && gameState.currentRow == 5 && wordIsValid guess

                progressNextRow =
                    isSubmittable && guess /= gameState.solution && gameState.currentRow < 6 && wordIsValid guess

                isUnsupportedWord =
                    isSubmittable && guess /= gameState.solution && not (wordIsValid guess)

                message =
                    if isUnsupportedWord then
                        Just "Not in word list"

                    else if not isSubmittable then
                        Just "Not enough letters"

                    else
                        Nothing

                board =
                    if progressNextRow then
                        applyGuess gameState

                    else
                        gameState.board

                newDict =
                    updateKeyboardDict gameState.currentGuess gameState.keyboardDictionary gameState.solution
            in
            if gameWon then
                ( GameEnd
                    { solution = gameState.solution
                    , board = gameState.board
                    , result = WonIn <| gameState.currentRow + 1
                    , message = Nothing -- get the message
                    , keyboardLetters = gameState.keyboardLetters
                    }
                , Cmd.none
                )

            else if gameLost then
                ( GameEnd
                    { solution = gameState.solution
                    , board = gameState.board
                    , result = Lost
                    , message = Just gameState.solution
                    , keyboardLetters = gameState.keyboardLetters
                    }
                , Cmd.none
                )

            else
                ( InProgress
                    { gameState
                        | currentRow =
                            if progressNextRow then
                                gameState.currentRow + 1

                            else
                                gameState.currentRow
                        , board = board
                        , currentGuess =
                            if not isUnsupportedWord then
                                []

                            else
                                gameState.currentGuess
                        , shakeRow =
                            if isUnsupportedWord || not isSubmittable then
                                Just gameState.currentRow

                            else
                                Nothing
                        , message = message
                        , keyboardDictionary = newDict
                    }
                , Cmd.batch [ clearAnimation (isUnsupportedWord || not isSubmittable), clearAlert message ]
                )

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

        ( InProgress gameState, ClearAnimation ) ->
            ( InProgress { gameState | shakeRow = Nothing }
            , Cmd.none
            )

        ( InProgress gameState, ClearAlert ) ->
            ( InProgress { gameState | message = Nothing }
            , Cmd.none
            )

        _ ->
            Debug.todo "handle rest"



-- Views


view : Model -> Html Msg
view model =
    case model of
        InProgress gameState ->
            let
                message =
                    case gameState.message of
                        Just messageText ->
                            div [ HA.class "message" ] [ text messageText ]

                        Nothing ->
                            text ""

                boardRows =
                    gameState.board
                        |> List.indexedMap (\idx row -> renderBoardRow idx gameState.shakeRow row)

                keyboardRows =
                    List.map (renderRow gameState.keyboardDictionary) gameState.keyboardLetters
            in
            div [ HA.class "app" ]
                [ div [ HA.class "board_wrapper" ]
                    [ message
                    , div [ HA.class "board" ] boardRows
                    ]
                , div [ HA.class "keyboard" ] keyboardRows
                ]

        _ ->
            div [] [ text "todo - handle other states" ]


renderBoardRow : Int -> Maybe Int -> KeyboardRow -> Html Msg
renderBoardRow idx shakeRowVal boardRow =
    div [ HA.class <| boardRowClass idx shakeRowVal ] (List.indexedMap (\index letter -> renderBoardRowItems index letter) boardRow)


renderBoardRowItems : Int -> Letter -> Html Msg
renderBoardRowItems idx letter =
    let
        charAsString =
            letter
                |> Tuple.first
                |> String.fromChar
    in
    div [ HA.class <| revealTileClass letter ]
        [ div
            [ HA.class "front"
            , HA.style "transition-delay" (withDelay (idx * 300))
            ]
            [ text charAsString ]
        , div
            [ HA.class "back"
            , HA.style "transition-delay" (withDelay (idx * 300))
            , HA.style "animation-delay" (withDelay (idx * 100))
            ]
            [ text charAsString ]
        ]


renderRow : KeyboardDictionary -> List Char -> Html Msg
renderRow keyboardDictionary letterRows =
    let
        key_rows =
            List.map (renderBtn keyboardDictionary) letterRows
    in
    div [ HA.class "keyboard_row" ] key_rows


renderBtn : KeyboardDictionary -> Char -> Html Msg
renderBtn keyboardDictionary letter =
    case letter of
        '⌫' ->
            button [ HA.class "key is_delete", onClick Delete ] [ String.fromChar letter |> text ]

        '⏎' ->
            button [ HA.class "key is_enter", onClick SubmitGuess ] [ String.fromChar letter |> text ]

        '0' ->
            div [ HA.class "key is_spacer" ] []

        char ->
            let
                state =
                    Dict.get char keyboardDictionary
            in
            button [ HA.class (keyClass char state), onClick <| KeyPress letter ] [ String.fromChar letter |> text ]



-- Game related functions


letterStateAsString : LetterState -> String
letterStateAsString letterState =
    case letterState of
        Blank ->
            "Blank"

        Pending ->
            "Pending"

        Incorrect ->
            "Incorrect"

        Correct ->
            "Correct"

        Present ->
            "Present"


getRandomWord : Cmd Msg
getRandomWord =
    let
        generator : Random.Generator Int
        generator =
            Random.int 0 wordLength
    in
    Random.generate GotRandomIndex generator


applyGuess : GameInProgress -> List KeyboardRow
applyGuess game =
    game.board
        |> List.indexedMap (markCorrect game.currentRow game.solution)
        |> List.indexedMap (markOtherTiles game.currentRow game.solution)


updateKeyboardDict : List Char -> KeyboardDictionary -> String -> KeyboardDictionary
updateKeyboardDict currentGuess currentDict solution =
    LE.indexedFoldl
        (\idx ch dict ->
            Dict.update ch
                (\maybeLetterState ->
                    let
                        checkCorrect =
                            checkCorrectChar ch idx solution

                        checkOthers =
                            checkOtherStatesChar ch solution
                    in
                    case maybeLetterState of
                        Just Blank ->
                            Just (checkCorrect Blank |> checkOthers)

                        -- if at any point is has been marked correct, we want to keep that demarcation on the keyboard
                        Just Correct ->
                            Just Correct

                        Just Present ->
                            Just (checkCorrect Present |> checkOthers)

                        Just Incorrect ->
                            Just (checkCorrect Incorrect |> checkOthers)

                        _ ->
                            Just Blank
                )
                dict
        )
        currentDict
        currentGuess


checkCorrectChar : Char -> Int -> String -> LetterState -> LetterState
checkCorrectChar ch idx word currentLetterState =
    let
        charAsString =
            String.fromChar ch

        occurrences =
            String.indexes charAsString word

        isCorrect =
            List.any (\occ -> occ == idx) occurrences
    in
    if isCorrect then
        Correct

    else
        currentLetterState


checkOtherStatesChar : Char -> String -> LetterState -> LetterState
checkOtherStatesChar ch word markedLetterState =
    let
        charAsString =
            String.fromChar ch

        occurrences =
            String.indexes charAsString word

        isPresent =
            List.length occurrences > 0

        currentlyCorrect =
            case markedLetterState of
                Correct ->
                    True

                _ ->
                    False
    in
    if currentlyCorrect then
        Correct

    else if isPresent then
        Present

    else
        Incorrect


markCorrect : Int -> String -> Int -> List ( Char, LetterState ) -> List ( Char, LetterState )
markCorrect activeGameRow solution boardIdx tiles =
    if boardIdx == activeGameRow then
        tiles
            |> List.indexedMap
                (\letterIdx tile ->
                    let
                        ( char, currentLetterState ) =
                            tile

                        letterState =
                            checkCorrectChar char letterIdx solution currentLetterState
                    in
                    ( char, letterState )
                )

    else
        tiles


markOtherTiles : Int -> String -> Int -> List ( Char, LetterState ) -> List ( Char, LetterState )
markOtherTiles activeGameRow solution boardIdx tiles =
    let
        calcNewSolution : ( Char, LetterState ) -> Char -> Char
        calcNewSolution tile solutionChar =
            case tile of
                ( _, Correct ) ->
                    '_'

                _ ->
                    solutionChar

        calculatedSolution =
            List.map2 calcNewSolution tiles (String.toList solution) |> String.fromList |> String.trim
    in
    if boardIdx == activeGameRow then
        tiles
            |> List.map
                (\tile ->
                    let
                        ( char, currentLetterState ) =
                            tile

                        newLetterState =
                            checkOtherStatesChar char calculatedSolution currentLetterState
                    in
                    ( char, newLetterState )
                )

    else
        tiles


initKeyboardDict : KeyboardDictionary
initKeyboardDict =
    Dict.fromList
        [ ( 'a', Blank )
        , ( 'b', Blank )
        , ( 'c', Blank )
        , ( 'd', Blank )
        , ( 'e', Blank )
        , ( 'f', Blank )
        , ( 'g', Blank )
        , ( 'h', Blank )
        , ( 'i', Blank )
        , ( 'j', Blank )
        , ( 'k', Blank )
        , ( 'l', Blank )
        , ( 'm', Blank )
        , ( 'n', Blank )
        , ( 'o', Blank )
        , ( 'p', Blank )
        , ( 'q', Blank )
        , ( 'r', Blank )
        , ( 's', Blank )
        , ( 't', Blank )
        , ( 'u', Blank )
        , ( 'v', Blank )
        , ( 'w', Blank )
        , ( 'x', Blank )
        , ( 'y', Blank )
        , ( 'z', Blank )
        ]


initKeyboardDictLetters : List (List Char)
initKeyboardDictLetters =
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


initBoard : List KeyboardRow
initBoard =
    List.repeat 6 <| List.repeat 5 ( ' ', Blank )



-- Helper function for dynamic classes


revealTileClass : Letter -> String
revealTileClass letter =
    case letter of
        ( _, Correct ) ->
            "tile reveal is_correct"

        ( _, Present ) ->
            "tile reveal is_present"

        ( _, Incorrect ) ->
            "tile reveal is_incorrect"

        ( _, Pending ) ->
            "tile filled"

        _ ->
            "tile"


keyClass : Char -> Maybe LetterState -> String
keyClass letter maybeLetterState =
    let
        state =
            case maybeLetterState of
                Just st ->
                    letterStateAsString st

                Nothing ->
                    letterStateAsString Blank
    in
    "key " ++ "is_" ++ String.fromChar letter ++ " " ++ "is_" ++ String.toLower state


boardRowClass : Int -> Maybe Int -> String
boardRowClass boardRowIdx shakeRow =
    case shakeRow of
        Just idx ->
            if boardRowIdx == idx then
                "board_row shake"

            else
                "board_row"

        Nothing ->
            "board_row"



-- Utils


withDelay : Int -> String
withDelay int =
    let
        delay =
            String.fromInt int
    in
    delay ++ "ms"


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


clearAnimation : Bool -> Cmd Msg
clearAnimation shouldClear =
    if shouldClear then
        Process.sleep 500 |> Task.perform (\_ -> ClearAnimation)

    else
        Cmd.none


clearAlert : Maybe String -> Cmd Msg
clearAlert maybeMessage =
    case maybeMessage of
        Just _ ->
            Process.sleep 1000 |> Task.perform (\_ -> ClearAlert)

        _ ->
            Cmd.none

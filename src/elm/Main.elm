{-
   Icon to represent instrument type
   Option to loop
   Keyboard shortcut for toggling values using left / right arrows and channel number
   Highlight follows playback
   Edit note volume
   Don't play again if in progress
   Save / Load
-}


module Main exposing (init)

import Html exposing (..)
import Html.Attributes exposing (class, classList, type_, checked, selected, value)
import Html.Events exposing (onClick, onInput)
import Random exposing (generate)
import Random.List exposing (shuffle)
import List
import String
import Utilities exposing (..)
import Types exposing (..)
import Edit exposing (EditMsg(..))
import PlayPort
import SelectList exposing (SelectList, before, after)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


noteFromId : Int -> Note
noteFromId id =
    (Note id Rest PointFive)


patternFromId : Int -> Pattern
patternFromId id =
    { initialPattern | id = id }


initialPattern : Pattern
initialPattern =
    let
        notes =
            [ noteFromId 1
            , noteFromId 2
            , noteFromId 3
            , noteFromId 4
            , noteFromId 5
            , noteFromId 6
            , noteFromId 7
            , noteFromId 8
            , noteFromId 9
            , noteFromId 10
            , noteFromId 11
            , noteFromId 12
            , noteFromId 13
            , noteFromId 14
            , noteFromId 15
            , noteFromId 16
            ]

        instruments =
            [ Instrument 0 "HiHat" False HiHat notes
            , Instrument 1 "Tom1" False Tom1 notes
            , Instrument 2 "Tom4" False Tom4 notes
            , Instrument 3 "Snare" False Snare notes
            , Instrument 4 "Kick" False Kick notes
            , Instrument 5 "Metronome" False Metronome notes
            ]
    in
        Pattern 0 "New Pattern" instruments 16 120 PlayOnce PlayMode Eighth


init : ( Model, Cmd Msg )
init =
    ( SelectList.singleton initialPattern, Cmd.none )


type Msg
    = ShuffledNotes Instrument (List Note)
    | Shuffle
    | Shift
    | CycleNote Instrument Note
    | ToggleSelected Instrument
    | ChangePatternLength String
    | ChangeTempo String
    | Play
    | EditSub String
    | EnableEdit
    | EditMsg EditMsg
    | AddPattern
    | FocusPattern Pattern



-- UPDATE


formatNote : Note -> String
formatNote note =
    case note.value of
        Rest ->
            "-"

        Accent ->
            ">"

        Hit ->
            "x"


serialize : List Instrument -> List ( String, List String )
serialize instruments =
    let
        groupNotes instrument =
            let
                pair list =
                    ( toString instrument.sound, list )
            in
                instrument.notes
                    |> List.map formatNote
                    |> pair
    in
        instruments
            |> List.map groupNotes


shuffleInstrumentNotes : Model -> ( Model, Cmd Msg )
shuffleInstrumentNotes model =
    let
        generateCmd instrument =
            if instrument.selected then
                generate (ShuffledNotes instrument) (shuffle instrument.notes)
            else
                Cmd.none

        cmds =
            List.map generateCmd (selectedInstruments model)
    in
        model ! cmds


updateInstrumentNotes : List Note -> Instrument -> Instrument
updateInstrumentNotes notes instrument =
    { instrument | notes = reposition notes }


shuffleNotes : Instrument -> List Note -> Model -> Model
shuffleNotes instrument shuffledNotes model =
    updateIf
        (matchesId instrument)
        (updateInstrumentNotes shuffledNotes)
        (selectedInstruments model)
        |> (updateInstrumentsInActiveModel model)



{- Take the last element of a List and move it to the front -}


wrap : List a -> List a
wrap xs =
    case List.reverse xs of
        [] ->
            []

        lastReversed :: restReversed ->
            lastReversed :: List.reverse restReversed


shift : Model -> Model
shift model =
    let
        shiftNotes instrument =
            updateInstrumentNotes (instrument.notes |> wrap) instrument
    in
        updateIf
            .selected
            shiftNotes
            (selectedInstruments model)
            |> (updateInstrumentsInActiveModel model)


selectedPattern : Model -> Pattern
selectedPattern model =
    SelectList.selected model


selectedInstruments : Model -> List Instrument
selectedInstruments model =
    (selectedPattern model).instruments


selectedTempo : Model -> Int
selectedTempo model =
    (selectedPattern model).tempo


selectedSubdivision : Model -> Subdivision
selectedSubdivision model =
    (selectedPattern model).subdivision


selectedInteractionMode : Model -> InteractionMode
selectedInteractionMode model =
    (selectedPattern model).interactionMode


selectedPatternLength : Model -> Int
selectedPatternLength model =
    (selectedPattern model).patternLength


updateSelectedInstrument : Instrument -> Model -> Model
updateSelectedInstrument instrument model =
    let
        toggleSelected currentInstrument =
            { currentInstrument | selected = not currentInstrument.selected }
    in
        updateIf
            (matchesId instrument)
            toggleSelected
            (selectedInstruments model)
            |> (updateInstrumentsInActiveModel model)


cycleNoteValue : Note -> Note
cycleNoteValue note =
    case note.value of
        Rest ->
            { note | value = Hit }

        Accent ->
            { note | value = Rest }

        Hit ->
            { note | value = Accent }


cycleInstrumentNotes : Note -> Instrument -> Instrument
cycleInstrumentNotes note instrument =
    let
        updated =
            updateIf
                (matches .position note)
                cycleNoteValue
                instrument.notes
    in
        { instrument | notes = updated }


cycleNote : Instrument -> Note -> Model -> Model
cycleNote instrument note model =
    updateIf
        (matchesId instrument)
        (cycleInstrumentNotes note)
        (selectedInstruments model)
        |> (updateInstrumentsInActiveModel model)


updateInstrumentsInPattern : Pattern -> List Instrument -> Pattern
updateInstrumentsInPattern pattern instruments =
    { pattern | instruments = instruments }


updateInstrumentsInActiveModel : Model -> List Instrument -> Model
updateInstrumentsInActiveModel model instruments =
    instruments
        |> updateInstrumentsInPattern (selectedPattern model)
        |> updatePatternInSelected model


updatePatternLength : String -> Model -> Model
updatePatternLength newPatternLength model =
    case String.toInt newPatternLength of
        Err _ ->
            model

        Ok val ->
            let
                oldPattern =
                    selectedPattern model

                newPattern =
                    { oldPattern | patternLength = val }
            in
                model
                    |> updateSelected newPattern
                    |> updateModelNotePositions


updateActiveSubdivision : Subdivision -> Model -> Model
updateActiveSubdivision subdivision model =
    let
        oldSelected =
            selectedPattern model

        newSelected =
            { oldSelected | subdivision = subdivision }
    in
        model |> updateSelected newSelected


updateSubdivision : String -> Model -> Model
updateSubdivision subdivisionInput model =
    case subdivisionInput of
        "Sixteenth" ->
            model |> updateActiveSubdivision Sixteenth

        "Eighth" ->
            model |> updateActiveSubdivision Eighth

        "Quarter" ->
            model |> updateActiveSubdivision Quarter

        _ ->
            model


updateTempo : String -> Model -> Model
updateTempo newTempo model =
    case String.toInt newTempo of
        Err _ ->
            model

        Ok val ->
            let
                oldSelected =
                    selectedPattern model

                newSelected =
                    { oldSelected | tempo = val }
            in
                model |> updateSelected newSelected


addPattern : Model -> Model
addPattern model =
    let
        newPattern =
            patternFromId (List.length (SelectList.toList model))
    in
        model
            |> SelectList.append [ newPattern ]
            |> focusPattern newPattern


focusPattern : Pattern -> Model -> Model
focusPattern pattern model =
    SelectList.select (matchesId pattern) model


updateSelected : Pattern -> Model -> Model
updateSelected pattern model =
    SelectList.fromLists (before model) pattern (after model)


updatePatternInSelected : Model -> Pattern -> Model
updatePatternInSelected =
    flip updateSelected


updateInteractionMode : InteractionMode -> Model -> Model
updateInteractionMode mode model =
    let
        oldPattern =
            selectedPattern model
    in
        model |> updateSelected ({ oldPattern | interactionMode = mode })


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Shuffle ->
            model |> shuffleInstrumentNotes

        ShuffledNotes instrument shuffledNotes ->
            ( model |> shuffleNotes instrument shuffledNotes, Cmd.none )

        Shift ->
            ( model |> shift, Cmd.none )

        ToggleSelected instrument ->
            ( model |> updateSelectedInstrument instrument, Cmd.none )

        CycleNote instrument note ->
            ( model |> cycleNote instrument note, Cmd.none )

        ChangePatternLength newPatternLength ->
            ( model |> updatePatternLength newPatternLength, Cmd.none )

        ChangeTempo newTempo ->
            ( model |> updateTempo newTempo, Cmd.none )

        EnableEdit ->
            ( model |> updateInteractionMode EditMode, Cmd.none )

        EditMsg subMsg ->
            ( model |> Edit.update subMsg, Cmd.none )

        Play ->
            let
                -- This is hacky
                portPlay =
                    (PlayPort.play
                        ( (serialize (selectedInstruments model))
                        , (selectedTempo model)
                        )
                    )
            in
                ( model, portPlay )

        EditSub newSubdivision ->
            ( model |> updateSubdivision newSubdivision, Cmd.none )

        AddPattern ->
            ( model |> addPattern, Cmd.none )

        FocusPattern pattern ->
            ( model |> focusPattern pattern, Cmd.none )



-- VIEW


noteType : Beat -> ( String, String )
noteType beat =
    case beat of
        Rest ->
            ( "-", "-" )

        Hit ->
            ( "-", "x" )

        Accent ->
            ( ">", "x" )


accentClass : Beat -> Attribute msg
accentClass beat =
    case beat of
        Rest ->
            class "beat__accent beat__accent--rest"

        Hit ->
            class "beat__accent beat__accent--hit"

        Accent ->
            class "beat__accent beat__accent--accent"


noteValue : Instrument -> Note -> Html Msg
noteValue instrument note =
    let
        ( accent, hit ) =
            noteType note.value
    in
        td
            [ class "beat__note-container"
            , onClick (CycleNote instrument note)
            ]
            [ div [ accentClass note.value ] [ text accent ]
            , div [ class "beat__note" ] [ text hit ]
            ]


viewAccentsAndPattern : Instrument -> Html Msg
viewAccentsAndPattern instrument =
    let
        beatSelector =
            th [ class "beat__selector" ]
                [ label [ class "checkbox" ]
                    [ input
                        [ class "beat__pattern-selector checkbox"
                        , type_ "checkbox"
                        , checked instrument.selected
                        , onClick (ToggleSelected instrument)
                        ]
                        []
                    , text instrument.name
                    ]
                ]

        values =
            instrument.notes
                |> List.map (noteValue instrument)
                |> (::) beatSelector
    in
        tr
            [ classList
                [ ( "beat__pattern-container", True )
                , ( "beat__pattern-container--selected", instrument.selected )
                ]
            ]
            values


viewCount : Model -> String -> Html Msg
viewCount model count =
    td [ class "beat__note-container" ]
        [ div [ class "beat__note beat__count" ] [ text count ]
        ]


countMeasures : List String -> List String -> List String
countMeasures list filler =
    let
        beatCount number =
            number :: filler
    in
        List.concatMap beatCount list


patternWrappedByMeasure : Int -> List String
patternWrappedByMeasure patternLength =
    let
        downbeatCount =
            (patternLength - 1)
                |> toFloat
                |> flip (/) 4
                |> ceiling

        patternRange =
            List.range 0 downbeatCount

        toDownbeat n =
            toString (n % 4 + 1)
    in
        List.map toDownbeat patternRange


viewPatterns : Model -> Html Msg
viewPatterns model =
    let
        beatSelector =
            td [ class "beat__selector" ] []

        downbeats =
            patternWrappedByMeasure (selectedPatternLength model)

        countWithSubdivision =
            countMeasures downbeats

        counts =
            case (selectedSubdivision model) of
                Sixteenth ->
                    List.take (selectedPatternLength model) (countWithSubdivision [ "e", "&", "a" ])

                Eighth ->
                    List.take (selectedPatternLength model) (countWithSubdivision [ "_", "&", "_" ])

                Quarter ->
                    List.take (selectedPatternLength model) (countWithSubdivision [ "_", "_", "_" ])

        countHelper =
            counts
                |> List.map (viewCount model)
                |> (::) beatSelector

        countHtml =
            tr [ class "beat__pattern-container" ] countHelper

        instrumentPatterns =
            (selectedInstruments model)
                |> List.map viewAccentsAndPattern
                |> flip (++) [ countHtml ]
    in
        table [ class "beat__container" ]
            [ tbody [] instrumentPatterns
            ]


viewButtonIcon : String -> String -> Msg -> Html Msg
viewButtonIcon name icon action =
    p [ class "control" ]
        [ button [ class "button", onClick action ]
            [ span [ class "icon" ] [ Html.i [ class ("fa fa-" ++ icon) ] [] ]
            , span [] [ text name ]
            ]
        ]


viewButton : String -> String -> Msg -> Html Msg
viewButton name className action =
    p [ class "control" ]
        [ button
            [ class ("button " ++ className), onClick action ]
            [ text name ]
        ]


hasSelected : List { a | selected : Bool } -> Bool
hasSelected selectable =
    List.any .selected selectable


viewShuffle : Model -> Html Msg
viewShuffle model =
    if hasSelected (selectedInstruments model) then
        viewButtonIcon "Shuffle" "random" Shuffle
    else
        text ""


viewShift : Model -> Html Msg
viewShift model =
    if hasSelected (selectedInstruments model) then
        viewButtonIcon "Shift" "angle-double-right" Shift
    else
        text ""


viewAddPattern : Html Msg
viewAddPattern =
    li [ onClick AddPattern ] [ a [] [ text "+" ] ]


viewPatternNameLinks : Model -> List (Html Msg)
viewPatternNameLinks model =
    let
        className pattern =
            if matchesId (selectedPattern model) pattern then
                "is-active"
            else
                ""

        link pattern =
            li
                [ class (className pattern)
                , onClick (FocusPattern pattern)
                ]
                [ a [] [ text pattern.name ] ]

        links =
            SelectList.map link model
    in
        SelectList.toList links ++ [ viewAddPattern ]


viewPatternNames : Model -> Html Msg
viewPatternNames model =
    div [ class "tabs" ]
        [ ul [] (viewPatternNameLinks model) ]


viewPlay : Model -> Html Msg
viewPlay model =
    section [ class "beat__play-container section" ]
        [ viewPatternNames model
        , viewPatterns model
        , div [ class "field is-grouped" ]
            [ viewButton "Play" "is-primary" Play
            , viewShuffle model
            , viewShift model
            , viewButton "Edit" "" EnableEdit
            ]
        , viewNavPanel model
        ]


viewPatternLengthInput : Int -> Html Msg
viewPatternLengthInput patternLength =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-normal" ]
            [ label [ class "label" ] [ text "Notes" ] ]
        , div [ class "field-body" ]
            [ div [ class "field" ]
                [ p [ class "control" ]
                    [ input
                        [ class "input"
                        , type_ "number"
                        , onInput ChangePatternLength
                        , value (toString patternLength)
                        , Html.Attributes.min "0"
                        ]
                        []
                    ]
                ]
            ]
        ]


viewTempoInput : Model -> Html Msg
viewTempoInput model =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-normal" ]
            [ label [ class "label" ] [ text "Tempo" ] ]
        , div [ class "field-body" ]
            [ div [ class "field" ]
                [ p [ class "control" ]
                    [ input
                        [ class "input"
                        , type_ "number"
                        , onInput ChangeTempo
                        , value (toString (((selectedPattern model)).tempo))
                        , Html.Attributes.min "0"
                        ]
                        []
                    ]
                ]
            ]
        ]


viewActiveSubdivision : Subdivision -> Subdivision -> Html Msg
viewActiveSubdivision activeSubdivision otherSubdivision =
    option
        [ selected (activeSubdivision == otherSubdivision) ]
        [ text (toString otherSubdivision) ]


viewCountHelper : Subdivision -> Html Msg
viewCountHelper subdivision =
    let
        options =
            [ Quarter, Eighth, Sixteenth ]
                |> List.map (viewActiveSubdivision subdivision)
    in
        div [ class "field is-horizontal" ]
            [ div [ class "field-label is-normal" ]
                [ label [ class "label" ] [ text "Counts" ] ]
            , div [ class "field-body" ]
                [ div [ class "field" ]
                    [ p [ class "control" ]
                        [ div [ class "select" ]
                            [ select [ onInput EditSub ] options
                            ]
                        ]
                    ]
                ]
            ]


viewNavPanel : Model -> Html Msg
viewNavPanel model =
    nav [ class "panel" ]
        [ p [ class "panel-heading" ] [ text "Settings" ]
        , a [ class "panel-block is-active" ] [ viewPatternLengthInput (selectedPatternLength model) ]
        , a [ class "panel-block is-active" ] [ viewTempoInput model ]
        , a [ class "panel-block is-active" ] [ viewCountHelper (selectedSubdivision model) ]
        ]


viewTitle : Html Msg
viewTitle =
    section [ class "hero is-primary" ]
        [ div [ class "hero-body" ]
            [ div [ class "container" ]
                [ h1 [ class "title" ] [ text "Beat Generator" ]
                , h2 [ class "subtitle" ] [ text "Create permutations of drum patterns" ]
                ]
            ]
        ]


viewMode : Model -> Html Msg
viewMode model =
    case (selectedInteractionMode model) of
        EditMode ->
            Edit.view model
                |> Html.map EditMsg

        PlayMode ->
            viewPlay model


viewFooter : Html Msg
viewFooter =
    footer [ class "footer" ]
        [ div [ class "container" ]
            [ div [ class "content has-text-centered" ]
                [ p []
                    [ strong [] [ text "Beat Generator" ]
                    , text " by "
                    , a [ Html.Attributes.href "http://diebo.lt" ] [ text "Matt Diebolt" ]
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ viewTitle
        , viewMode model
        , viewFooter
        ]

module PatternEditor exposing (view, update, PatternEditorMsg(..))

import Types exposing (..)
import Random exposing (generate)
import Random.List exposing (shuffle)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pattern exposing (..)
import Utilities exposing (..)


-- MODEL


type PatternEditorMsg
    = CycleNote Instrument Note
    | SetPatternLength String
    | SetSubdivision String
    | SetTempo String
    | Shift
    | Shuffle
    | ShuffledNotes Instrument (List Note)
    | ToggleSelected Instrument



-- UPDATE


shuffleInstrumentNotes : Pattern -> Cmd PatternEditorMsg
shuffleInstrumentNotes pattern =
    let
        generateCmd instrument =
            if instrument.selected then
                generate (ShuffledNotes instrument) (shuffle instrument.notes)
            else
                Cmd.none

        cmds =
            List.map generateCmd (getInstruments pattern)
    in
        Cmd.batch cmds


updateInstrumentNotes : List Note -> Instrument -> Instrument
updateInstrumentNotes notes instrument =
    { instrument | notes = reposition notes }


shuffleNotes : Instrument -> List Note -> Pattern -> Pattern
shuffleNotes instrument shuffledNotes pattern =
    let
        newInstruments =
            updateIf
                (matchesId instrument)
                (updateInstrumentNotes shuffledNotes)
                (getInstruments pattern)
    in
        pattern |> setInstruments newInstruments



{- Take the last element of a List and move it to the front -}


wrap : List a -> List a
wrap xs =
    case List.reverse xs of
        [] ->
            []

        lastReversed :: restReversed ->
            lastReversed :: List.reverse restReversed


shift : Pattern -> Pattern
shift pattern =
    let
        shiftNotes instrument =
            updateInstrumentNotes (instrument.notes |> wrap) instrument

        newInstruments =
            updateIf
                .selected
                shiftNotes
                (getInstruments pattern)
    in
        pattern |> setInstruments newInstruments


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


cycleNote : Instrument -> Note -> Pattern -> Pattern
cycleNote instrument note pattern =
    let
        newInstruments =
            updateIf
                (matchesId instrument)
                (cycleInstrumentNotes note)
                (getInstruments pattern)
    in
        pattern |> setInstruments newInstruments


update : PatternEditorMsg -> Pattern -> ( Pattern, Cmd PatternEditorMsg )
update msg pattern =
    case msg of
        Shuffle ->
            ( pattern, pattern |> shuffleInstrumentNotes )

        ShuffledNotes instrument shuffledNotes ->
            ( pattern |> shuffleNotes instrument shuffledNotes, Cmd.none )

        Shift ->
            ( pattern |> shift, Cmd.none )

        ToggleSelected instrument ->
            ( pattern |> updateSelectedInstrument instrument, Cmd.none )

        CycleNote instrument note ->
            ( pattern |> cycleNote instrument note, Cmd.none )

        SetPatternLength newPatternLength ->
            ( pattern |> updatePatternLengthFromInput newPatternLength, Cmd.none )

        SetTempo newTempo ->
            ( pattern |> updateTempoFromInput newTempo, Cmd.none )

        SetSubdivision newSubdivision ->
            ( pattern |> updateSubdivisionFromInput newSubdivision, Cmd.none )



-- VIEW


view : Pattern -> Html PatternEditorMsg
view pattern =
    div []
        [ viewPattern pattern
        , div [ class "field is-grouped" ]
            [ viewShuffle pattern
            , viewShift pattern
            ]
        , viewNavPanel pattern
        ]


countMeasures : List String -> List String -> List String
countMeasures list filler =
    let
        beatCount number =
            number :: filler
    in
        List.concatMap beatCount list


viewPattern : Pattern -> Html PatternEditorMsg
viewPattern pattern =
    let
        beatSelector =
            td [ class "beat__selector" ] []

        downbeats =
            patternWrappedByMeasure (getPatternLength pattern)

        countWithSubdivision =
            countMeasures downbeats

        counts =
            case (getSubdivision pattern) of
                Sixteenth ->
                    List.take (getPatternLength pattern) (countWithSubdivision [ "e", "&", "a" ])

                Eighth ->
                    List.take (getPatternLength pattern) (countWithSubdivision [ "_", "&", "_" ])

                Quarter ->
                    List.take (getPatternLength pattern) (countWithSubdivision [ "_", "_", "_" ])

        countHelper =
            counts
                |> List.map (viewCount pattern)
                |> (::) beatSelector

        countHtml =
            tr [ class "beat__pattern-container" ] countHelper

        instrumentPatterns =
            (getInstruments pattern)
                |> List.map viewAccentsAndPattern
                |> flip (++) [ countHtml ]
    in
        table [ class "beat__container" ]
            [ tbody [] instrumentPatterns
            ]


viewAccentsAndPattern : Instrument -> Html PatternEditorMsg
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


viewButtonIcon : String -> String -> PatternEditorMsg -> Html PatternEditorMsg
viewButtonIcon name icon action =
    p [ class "control" ]
        [ button [ class "button", onClick action ]
            [ span [ class "icon" ] [ Html.i [ class ("fa fa-" ++ icon) ] [] ]
            , span [] [ text name ]
            ]
        ]


hasSelected : List { a | selected : Bool } -> Bool
hasSelected selectable =
    List.any .selected selectable


viewShuffle : Pattern -> Html PatternEditorMsg
viewShuffle pattern =
    if hasSelected (getInstruments pattern) then
        viewButtonIcon "Shuffle" "random" Shuffle
    else
        text ""


viewShift : Pattern -> Html PatternEditorMsg
viewShift pattern =
    if hasSelected (getInstruments pattern) then
        viewButtonIcon "Shift" "angle-double-right" Shift
    else
        text ""


viewNavPanel : Pattern -> Html PatternEditorMsg
viewNavPanel pattern =
    nav [ class "panel" ]
        [ p [ class "panel-heading" ] [ text "Settings" ]
        , a [ class "panel-block is-active" ] [ viewPatternLengthInput (getPatternLength pattern) ]
        , a [ class "panel-block is-active" ] [ viewTempoInput pattern ]
        , a [ class "panel-block is-active" ] [ viewCountHelper (getSubdivision pattern) ]
        ]


viewPatternLengthInput : Int -> Html PatternEditorMsg
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
                        , onInput SetPatternLength
                        , value (toString patternLength)
                        , Html.Attributes.min "0"
                        ]
                        []
                    ]
                ]
            ]
        ]


viewTempoInput : Pattern -> Html PatternEditorMsg
viewTempoInput pattern =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-normal" ]
            [ label [ class "label" ] [ text "Tempo" ] ]
        , div [ class "field-body" ]
            [ div [ class "field" ]
                [ p [ class "control" ]
                    [ input
                        [ class "input"
                        , type_ "number"
                        , onInput SetTempo
                        , value (toString <| pattern.tempo)
                        , Html.Attributes.min "0"
                        ]
                        []
                    ]
                ]
            ]
        ]


viewCount : Pattern -> String -> Html PatternEditorMsg
viewCount _ count =
    td [ class "beat__note-container" ]
        [ div [ class "beat__note beat__count" ] [ text count ]
        ]


viewCountHelper : Subdivision -> Html PatternEditorMsg
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
                            [ select [ onInput SetSubdivision ] options
                            ]
                        ]
                    ]
                ]
            ]


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


noteValue : Instrument -> Note -> Html PatternEditorMsg
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


viewActiveSubdivision : Subdivision -> Subdivision -> Html PatternEditorMsg
viewActiveSubdivision activeSubdivision otherSubdivision =
    option
        [ selected (activeSubdivision == otherSubdivision) ]
        [ text (toString otherSubdivision) ]

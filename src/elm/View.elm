module View exposing (view)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, classList, type_, checked, selected, value)
import Html.Events exposing (onClick, onInput)
import Edit exposing (view)
import SelectList exposing (SelectList)
import Utilities exposing (matchesId)


selectedInstruments : Model -> List Instrument
selectedInstruments model =
    (selectedPattern model).instruments


selectedPattern : Model -> Pattern
selectedPattern model =
    SelectList.selected model


selectedSubdivision : Model -> Subdivision
selectedSubdivision model =
    (selectedPattern model).subdivision


selectedPatternLength : Model -> Int
selectedPatternLength model =
    (selectedPattern model).patternLength


selectedInteractionMode : Model -> InteractionMode
selectedInteractionMode model =
    (selectedPattern model).interactionMode


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

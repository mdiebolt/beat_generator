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
import Html.Events as Events
import Html.Attributes exposing (..)
import List
import Utilities
import Types exposing (..)
import InstrumentEditor
import PatternEditor
import Pattern exposing (..)
import SelectList


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


initialModel : Model
initialModel =
    SelectList.singleton initialPattern


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


patternsLength : Model -> Int
patternsLength model =
    List.length (SelectList.toList model)


addPattern : Model -> Model
addPattern model =
    let
        newPattern =
            patternFromId (patternsLength model)
    in
        model
            |> SelectList.append [ newPattern ]
            |> focusPattern newPattern


focusPattern : Pattern -> Model -> Model
focusPattern pattern model =
    SelectList.select (Utilities.matchesId pattern) model


updateInteractionModeFromInput : InteractionMode -> Model -> Model
updateInteractionModeFromInput mode model =
    let
        pattern =
            selectedPattern model

        newPattern =
            setInteractionMode mode pattern
    in
        setPattern newPattern model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddPattern ->
            ( model |> addPattern, Cmd.none )

        EnableEdit ->
            ( model |> updateInteractionModeFromInput EditMode, Cmd.none )

        FocusPattern pattern ->
            ( model |> focusPattern pattern, Cmd.none )

        -- Edit
        InstrumentEditorMsg subMsg ->
            let
                newPattern =
                    InstrumentEditor.update subMsg (selectedPattern model)
            in
                ( model |> setPattern newPattern, Cmd.none )

        -- Pattern Editor
        PatternEditorMsg subMsg ->
            let
                newPattern =
                    PatternEditor.update subMsg (selectedPattern model)
            in
                ( model |> setPattern newPattern, Cmd.none )



-- View


viewAddPattern : Html Msg
viewAddPattern =
    li [ Events.onClick AddPattern ] [ a [] [ text "+" ] ]


viewPatternNameLinks : Model -> List (Html Msg)
viewPatternNameLinks model =
    let
        className pattern =
            if Utilities.matchesId (selectedPattern model) pattern then
                "is-active"
            else
                ""

        link pattern =
            li
                [ class (className pattern)
                , Events.onClick (FocusPattern pattern)
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
        , PatternEditor.view (model |> selectedPattern) |> Html.map PatternEditorMsg
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
    case (getInteractionMode (selectedPattern model)) of
        EditMode ->
            InstrumentEditor.view (selectedPattern model)
                |> Html.map InstrumentEditorMsg

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

module Types exposing (..)

import SelectList exposing (SelectList)


type alias Model =
    SelectList Pattern


type alias Pattern =
    { id : Int
    , name : String
    , instruments : List Instrument
    , patternLength : Int
    , tempo : Int
    , playbackMode : Playback
    , interactionMode : InteractionMode
    , subdivision : Subdivision
    }


type alias Instrument =
    { id : Int
    , name : String
    , selected : Bool
    , sound : Sample
    , notes : List Note
    }


type alias Note =
    { position : Int
    , value : Beat
    , volume : Volume
    }


type Volume
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten


type InteractionMode
    = EditMode
    | PlayMode


type Playback
    = PlayOnce
    | PlayLoop


type Beat
    = Rest
    | Accent
    | Hit


type Sample
    = HiHat
    | Snare
    | Kick
    | Tom1
    | Tom2
    | Tom3
    | Tom4
    | Metronome



-- TODO: triplet subdivisions


type Subdivision
    = Sixteenth
    | Eighth
    | Quarter



-- Messages


type Msg
    = AddPattern
    | EnableEdit
    | FocusPattern Pattern
    | InstrumentEditorMsg InstrumentEditorMsg
    | PatternEditorMsg PatternEditorMsg


type InstrumentEditorMsg
    = AddInstrument
    | BeatName String
    | InstrumentName Instrument String
    | RemoveInstrument Instrument
    | SaveChanges
    | SelectAudioSound Instrument String


type PatternEditorMsg
    = CycleNote Instrument Note
    | Play
    | SetPatternLength String
    | SetSubdivision String
    | SetTempo String
    | Shift
    | Shuffle
    | ShuffledNotes Instrument (List Note)
    | ToggleSelected Instrument

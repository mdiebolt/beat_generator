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



--


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


type EditMsg
    = BeatName String
    | InstrumentName Instrument String
    | AddInstrument
    | RemoveInstrument Instrument
    | SaveChanges
    | SelectAudioSound Instrument String

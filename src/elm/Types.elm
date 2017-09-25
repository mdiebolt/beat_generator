module Types exposing (..)


type alias Model =
    { name : String
    , instruments : List Instrument
    , slots : Int
    , tempo : Int
    , editMode : Bool
    }


type alias Instrument =
    { name : String
    , selected : Bool
    , notes : List Note
    }


type alias Note =
    { position : Int
    , value : Beat
    }


type Beat
    = Rest
    | Accent
    | Hit


type
    Msg
    -- Notes
    = ShuffledNotes Instrument (List Note)
    | Shuffle
    | Shift
      -- Note
    | CycleNote Instrument Note
      -- Instrument
    | ToggleSelected Instrument
      -- Playback
    | ChangeSubdivision String
    | ChangeTempo String
    | Play
      -- Pre Edit
    | EditBeat
      -- Edit
    | ChangeBeatName String
    | ChangeInstrumentName Instrument String
    | AddInstrument
    | RemoveInstrument Instrument
    | SaveChanges

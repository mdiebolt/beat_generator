port module PlayPort exposing (play, serialize)

import Types exposing (..)
import List


port play : ( List ( String, List String ), Int ) -> Cmd msg


serialize : List Instrument -> List ( String, List String )
serialize instruments =
    List.map groupInstrumentNameAndFormattedNotes instruments


groupInstrumentNameAndFormattedNotes : Instrument -> ( String, List String )
groupInstrumentNameAndFormattedNotes instrument =
    instrument.notes
        |> List.map formatNote
        |> pairInstrumentSoundAndNotes instrument


formatNote : Note -> String
formatNote note =
    case note.value of
        Rest ->
            "-"

        Accent ->
            ">"

        Hit ->
            "x"


pairInstrumentSoundAndNotes : Instrument -> List String -> ( String, List String )
pairInstrumentSoundAndNotes instrument formattedNotes =
    ( toString instrument.sound, formattedNotes )

module Version exposing (Version, decode, default, toString)

{-| Manage the current version of the app.
-}

import Json.Decode


type Version
    = Version String


decode : Json.Decode.Decoder Version
decode =
    Json.Decode.map Version Json.Decode.string


default : Version
default =
    Version "1.0.0"


toString : Version -> String
toString (Version version) =
    version

module Avatar exposing (Avatar, decode, empty, encode, selectionSet, toMaybeString, view)

import Asset.Icon as Icon
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html)
import Html.Attributes exposing (class, src)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



-- Avatar


type Avatar
    = Avatar (Maybe String)


empty : Avatar
empty =
    Avatar Nothing


decode : Decoder Avatar
decode =
    Decode.map Avatar (Decode.nullable Decode.string)


encode : Avatar -> Value
encode (Avatar maybeUrl) =
    case maybeUrl of
        Just url ->
            Encode.string url

        Nothing ->
            Encode.null


view : Avatar -> String -> Html msg
view (Avatar maybeUrl) cls =
    case maybeUrl of
        Nothing ->
            Icon.accountCircle cls

        Just url ->
            if String.isEmpty (String.trim url) then
                Icon.accountCircle cls

            else
                Html.div
                    [ class ("profile-avatar " ++ cls)
                    ]
                    [ Html.img
                        [ class ("profile-avatar object-cover " ++ cls)
                        , src url
                        ]
                        []
                    ]


selectionSet : SelectionSet (Maybe String) typeLock -> SelectionSet Avatar typeLock
selectionSet =
    SelectionSet.map Avatar


toMaybeString : Avatar -> Maybe String
toMaybeString (Avatar s) =
    s

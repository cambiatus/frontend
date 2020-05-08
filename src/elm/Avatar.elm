module Avatar exposing (Avatar, decode, empty, encode, selectionSet, toMaybeString, view, viewNoAnchor)

import Asset.Icon as Icon
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (class, href, src, style)
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
encode (Avatar maybeHash) =
    case maybeHash of
        Just hash ->
            Encode.string hash

        Nothing ->
            Encode.null


view : String -> Avatar -> String -> String -> Html msg
view url (Avatar maybeHash) username cls =
    Html.a
        [ href ("/profile/" ++ username)
        ]
        [ viewIcon url (Avatar maybeHash) cls ]


viewIcon : String -> Avatar -> String -> Html msg
viewIcon url (Avatar maybeHash) cls =
    case maybeHash of
        Nothing ->
            Icon.accountCircle cls

        Just hash ->
            if String.isEmpty (String.trim hash) then
                Icon.accountCircle cls

            else
                Html.a
                    [ class ("profile-avatar " ++ cls)
                    ]
                    [ Html.img
                        [ class ("profile-avatar object-cover " ++ cls)
                        , src (url ++ "/" ++ hash)
                        ]
                        []
                    ]


viewNoAnchor : String -> Avatar -> String -> Html msg
viewNoAnchor url (Avatar maybeHash) cls =
    Html.div []
        [ viewIcon url (Avatar maybeHash) cls
        ]


selectionSet : SelectionSet (Maybe String) typeLock -> SelectionSet Avatar typeLock
selectionSet =
    SelectionSet.map Avatar


toMaybeString : Avatar -> Maybe String
toMaybeString (Avatar s) =
    s

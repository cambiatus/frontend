module Avatar exposing
    ( Avatar
    , decode
    , empty
    , selectionSet
    , toMaybeString
    , view
    )

import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html)
import Html.Attributes exposing (class, src)
import Icons
import Json.Decode as Decode exposing (Decoder)



-- Avatar


type Avatar
    = Avatar (Maybe String)


empty : Avatar
empty =
    Avatar Nothing


decode : Decoder Avatar
decode =
    Decode.map Avatar (Decode.nullable Decode.string)


view : Avatar -> String -> Html msg
view (Avatar maybeUrl) cls =
    case maybeUrl of
        Nothing ->
            Icons.accountCircle cls

        Just url ->
            if String.isEmpty (String.trim url) then
                Icons.accountCircle cls

            else
                Html.div
                    [ class cls
                    ]
                    [ Html.img
                        [ class ("object-cover rounded-full " ++ cls)
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

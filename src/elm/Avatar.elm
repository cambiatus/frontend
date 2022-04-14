module Avatar exposing
    ( Avatar
    , empty
    , fromString
    , selectionSet
    , toMaybeString
    , view
    )

import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html)
import Html.Attributes exposing (alt, class, src)
import Icons



-- Avatar


type Avatar
    = Avatar (Maybe String)


empty : Avatar
empty =
    Avatar Nothing


view : Avatar -> String -> Html msg
view (Avatar maybeUrl) cls =
    case maybeUrl of
        Nothing ->
            Icons.accountCircleSolid cls

        Just url ->
            if String.isEmpty (String.trim url) then
                Icons.accountCircleSolid cls

            else
                Html.div
                    [ class cls
                    ]
                    [ Html.img
                        [ class ("object-cover rounded-full " ++ cls)
                        , src url
                        , alt ""
                        ]
                        []
                    ]


selectionSet : SelectionSet (Maybe String) typeLock -> SelectionSet Avatar typeLock
selectionSet =
    SelectionSet.map Avatar


toMaybeString : Avatar -> Maybe String
toMaybeString (Avatar s) =
    s


fromString : String -> Avatar
fromString =
    Just >> Avatar

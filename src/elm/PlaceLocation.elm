module PlaceLocation exposing
    ( Reply(..), replyDecoder
    , Status, status, locate, showAddressNote
    )

{-| A member's public place follows their own choice, else the browser's
location, else their default address. On the dashboard the browser is asked for
its location (see `src/utils/memberLocation.js`, which mirrors the LiveView
hook) and the answer is sent to the backend with `locatePlace`. Then, when the
member has no default address, a small note invites them to add one.

None of this blocks the dashboard: every failure is silent.


# The browser's answer

@docs Reply, replyDecoder


# The backend's answer

@docs Status, status, locate, showAddressNote

-}

import Cambiatus.Enum.PlaceSource exposing (PlaceSource)
import Cambiatus.Mutation as Mutation
import Cambiatus.Object
import Cambiatus.Object.PlaceStatus as PlaceStatus
import Cambiatus.Query as Query
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder)



-- THE BROWSER


{-| What the browser said, and whether the address note was already dismissed
this session

  - `Located`: a position (already rounded to about 110 m)
  - `Unavailable`: refused, or no geolocation: the place follows the address
  - `Skipped`: nothing to send now (already asked this session, hasn't moved,
    timed out); the place stays as it is

-}
type Reply
    = Located { latitude : Float, longitude : Float } { noteDismissed : Bool }
    | Unavailable { noteDismissed : Bool }
    | Skipped { noteDismissed : Bool }


replyDecoder : Decoder Reply
replyDecoder =
    Decode.map2 (\kind dismissed -> ( kind, dismissed ))
        (Decode.field "kind" Decode.string)
        (Decode.oneOf [ Decode.field "noteDismissed" Decode.bool, Decode.succeed False ])
        |> Decode.andThen
            (\( kind, dismissed ) ->
                let
                    flags =
                        { noteDismissed = dismissed }
                in
                case kind of
                    "located" ->
                        Decode.map2 (\latitude longitude -> Located { latitude = latitude, longitude = longitude } flags)
                            (Decode.field "lat" Decode.float)
                            (Decode.field "lng" Decode.float)

                    "unavailable" ->
                        Decode.succeed (Unavailable flags)

                    _ ->
                        Decode.succeed (Skipped flags)
            )



-- THE BACKEND


{-| Where the member's public place comes from, and whether they have a default
address
-}
type alias Status =
    { label : Maybe String
    , source : PlaceSource
    , hasDefaultAddress : Bool
    }


selectionSet : SelectionSet Status Cambiatus.Object.PlaceStatus
selectionSet =
    SelectionSet.succeed Status
        |> SelectionSet.with PlaceStatus.label
        |> SelectionSet.with PlaceStatus.source
        |> SelectionSet.with PlaceStatus.hasDefaultAddress


status : SelectionSet (Maybe Status) RootQuery
status =
    Query.placeStatus selectionSet


{-| Without a position (refused or none) the backend makes the place follow the
default address
-}
locate : Maybe { latitude : Float, longitude : Float } -> SelectionSet Status RootMutation
locate position =
    Mutation.locatePlace
        (\optionals ->
            case position of
                Just { latitude, longitude } ->
                    { optionals | latitude = Present latitude, longitude = Present longitude }

                Nothing ->
                    optionals
        )
        selectionSet


{-| The note shows for members without a default address, until they dismiss it
-}
showAddressNote : { dismissed : Bool } -> Maybe Status -> Bool
showAddressNote { dismissed } maybeStatus =
    case maybeStatus of
        Just { hasDefaultAddress } ->
            not hasDefaultAddress && not dismissed

        Nothing ->
            False

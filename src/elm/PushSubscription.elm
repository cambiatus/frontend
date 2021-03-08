module PushSubscription exposing (Keys, PushSubscription, activatePushMutation, decode)

import Cambiatus.Mutation as Mutation
import Eos.Account as Eos
import Graphql.Operation exposing (RootMutation)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)


type alias Keys =
    { p256dh : String
    , auth : String
    }


type alias PushSubscription =
    { endpoint : String
    , keys : Keys
    }


keysDecoder : Decoder Keys
keysDecoder =
    Decode.succeed Keys
        |> required "p256dh" string
        |> required "auth" string


decode : Decoder PushSubscription
decode =
    Decode.map2 PushSubscription
        (Decode.field "endpoint" string)
        (Decode.field "keys" keysDecoder)


activatePushMutation : Eos.Name -> PushSubscription -> SelectionSet () RootMutation
activatePushMutation name { endpoint, keys } =
    let
        input =
            { input = { authKey = keys.auth, pKey = keys.p256dh, endpoint = endpoint } }
    in
    Mutation.registerPush input SelectionSet.empty
        |> SelectionSet.map (\_ -> ())

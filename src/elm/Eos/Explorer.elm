module Eos.Explorer exposing (Subject(..), link)

import Eos.Account
import Session.Shared as Shared
import Transfer
import Url.Builder



-- BUILDING A LINK


type Subject
    = Profile Eos.Account.Name
    | Transfer Transfer.CreatedTx


link : Shared.Environment -> Subject -> String
link environment subject =
    Url.Builder.crossOrigin baseUrl
        (path subject)
        [ Url.Builder.string "nodeUrl" (nodeUrl environment)
        , Url.Builder.string "systemDomain" "eosio"
        , Url.Builder.string "coreSymbol" (coreSymbol environment)
        ]



-- INTERNAL HELPERS


baseUrl : String
baseUrl =
    "https://local.bloks.io"


path : Subject -> List String
path subject =
    case subject of
        Profile accountName ->
            [ "account", Eos.Account.nameToString accountName ]

        Transfer createdTx ->
            [ "transaction", Transfer.createdTxToString createdTx ]


nodeUrl : Shared.Environment -> String
nodeUrl environment =
    case environment of
        Shared.Development ->
            "https://staging.cambiatus.io"

        Shared.Staging ->
            "https://staging.cambiatus.io"

        Shared.Demo ->
            "https://demo.cambiatus.io"

        Shared.Production ->
            "https://app.cambiatus.io"


coreSymbol : Shared.Environment -> String
coreSymbol environment =
    case environment of
        Shared.Development ->
            "SYS"

        Shared.Staging ->
            "SYS"

        Shared.Demo ->
            "SYS"

        Shared.Production ->
            "EOS"

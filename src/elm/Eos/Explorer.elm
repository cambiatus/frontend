module Eos.Explorer exposing (Subject(..), link)

import Environment exposing (Environment)
import Eos.Account
import Transfer
import Url.Builder



-- BUILDING A LINK


type Subject
    = Profile Eos.Account.Name
    | Transfer Transfer.CreatedTx


link : Environment -> Subject -> String
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


nodeUrl : Environment -> String
nodeUrl environment =
    case environment of
        Environment.Development ->
            "https://staging.cambiatus.io"

        Environment.Staging ->
            "https://staging.cambiatus.io"

        Environment.Demo ->
            "https://demo.cambiatus.io"

        Environment.Production ->
            "https://app.cambiatus.io"


coreSymbol : Environment -> String
coreSymbol environment =
    case environment of
        Environment.Development ->
            "SYS"

        Environment.Staging ->
            "SYS"

        Environment.Demo ->
            "SYS"

        Environment.Production ->
            "EOS"

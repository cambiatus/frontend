module Page.Register.DefaultForm exposing (FormInput, FormOutput, decoder, encode, form, init)

import Eos.Account
import Form
import Json.Decode as Decode
import Json.Encode as Encode
import Page.Register.Common as Common
import Session.Shared exposing (Translators)
import Set exposing (Set)


type alias FormInput =
    { name : String
    , account : String
    , email : String
    }


type alias FormOutput =
    { name : String
    , account : Eos.Account.Name
    , email : String
    }


encode : FormOutput -> Encode.Value
encode output =
    Encode.object
        [ ( "name", Encode.string output.name )
        , ( "account", Eos.Account.encodeName output.account )
        , ( "email", Encode.string output.email )
        ]


decoder : Decode.Decoder FormOutput
decoder =
    Decode.map3 FormOutput
        (Decode.field "name" Decode.string)
        (Decode.field "account" Eos.Account.nameDecoder)
        (Decode.field "email" Decode.string)


form : Translators -> { unavailableAccounts : Set String } -> Form.Form msg FormInput FormOutput
form translators unavailableAccounts =
    Form.succeed FormOutput
        |> Form.with (Common.personNameField translators)
        |> Form.with (Common.accountNameField translators unavailableAccounts)
        |> Form.with (Common.emailField translators)


init : Form.Model FormInput
init =
    Form.init
        { name = ""
        , account = ""
        , email = ""
        }

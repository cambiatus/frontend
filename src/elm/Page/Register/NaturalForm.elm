module Page.Register.NaturalForm exposing (FormInput, FormOutput, decoder, documentTypeToString, encode, form, init)

import Eos.Account
import Form
import Form.Select
import Form.Text
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Json.Decode.Pipeline
import Json.Encode as Encode
import Page.Register.Common as Common
import Session.Shared exposing (Translators)
import Set exposing (Set)


type alias FormInput =
    { documentType : Document
    , document : String
    , name : String
    , email : String
    , phoneNumber : String
    , account : String
    }


init : Form.Model FormInput
init =
    Form.init
        { documentType = SSN
        , document = ""
        , name = ""
        , email = ""
        , phoneNumber = ""
        , account = ""
        }


type alias FormOutput =
    { documentType : Document
    , document : String
    , name : String
    , email : String
    , phoneNumber : String
    , account : Eos.Account.Name
    }


encode : FormOutput -> Encode.Value
encode output =
    Encode.object
        [ ( "documentType", Encode.string <| documentTypeToString output.documentType )
        , ( "document", Encode.string output.document )
        , ( "name", Encode.string output.name )
        , ( "email", Encode.string output.email )
        , ( "phoneNumber", Encode.string output.phoneNumber )
        , ( "account", Eos.Account.encodeName output.account )
        ]


decoder : Decode.Decoder FormOutput
decoder =
    Decode.succeed FormOutput
        |> Json.Decode.Pipeline.required "documentType"
            (Decode.string
                |> Decode.andThen
                    (\documentType ->
                        case documentTypeFromString documentType of
                            Nothing ->
                                Decode.fail "Invalid document type"

                            Just doc ->
                                Decode.succeed doc
                    )
            )
        |> Json.Decode.Pipeline.required "document" Decode.string
        |> Json.Decode.Pipeline.required "name" Decode.string
        |> Json.Decode.Pipeline.required "email" Decode.string
        |> Json.Decode.Pipeline.required "phoneNumber" Decode.string
        |> Json.Decode.Pipeline.required "account" Eos.Account.nameDecoder


form : Translators -> { unavailableAccounts : Set String } -> Form.Form msg FormInput FormOutput
form ({ t } as translators) unavailableAccounts =
    Form.succeed FormOutput
        |> Form.with
            (Form.Select.init
                { label = t "register.form.document.type"
                , id = "document-type-select"
                , optionToString = documentTypeToString
                }
                |> Form.Select.withOption SSN (t "register.form.document.cedula_de_identidad.label")
                |> Form.Select.withOption NITE (t "register.form.document.nite.label")
                |> Form.Select.withOption DIMEX (t "register.form.document.dimex.label")
                |> Form.Select.withContainerAttrs [ class "mb-10" ]
                |> Form.select (documentTypeFromString >> Maybe.withDefault SSN)
                    { parser = Ok
                    , value = .documentType
                    , update = \documentType input -> { input | documentType = documentType }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            ((\{ documentType } ->
                let
                    documentLength =
                        case documentType of
                            SSN ->
                                9

                            NITE ->
                                10

                            DIMEX ->
                                12
                in
                Form.Text.init
                    { label = t ("register.form.document." ++ documentTypeToString documentType ++ ".label")
                    , id = "document-input"
                    }
                    |> Form.Text.withCounter (Form.Text.CountLetters documentLength)
                    |> Form.Text.withPlaceholder (t ("register.form.document." ++ documentTypeToString documentType ++ ".placeholder"))
                    |> Form.textField
                        { parser = Ok
                        , value = .document
                        , update = \document input -> { input | document = document }
                        , externalError = always Nothing
                        }
             )
                |> Form.introspect
            )
        |> Form.with (Common.personNameField translators)
        |> Form.with (Common.emailField translators)
        |> Form.with (Common.phoneNumberField translators)
        |> Form.with (Common.accountNameField translators unavailableAccounts)



-- DOCUMENT


type Document
    = SSN
    | DIMEX
    | NITE


documentTypeToString : Document -> String
documentTypeToString document =
    case document of
        SSN ->
            "cedula_de_identidad"

        DIMEX ->
            "dimex"

        NITE ->
            "nite"


documentTypeFromString : String -> Maybe Document
documentTypeFromString document =
    case document of
        "cedula_de_identidad" ->
            Just SSN

        "dimex" ->
            Just DIMEX

        "nite" ->
            Just NITE

        _ ->
            Nothing

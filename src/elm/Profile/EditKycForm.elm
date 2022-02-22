module Profile.EditKycForm exposing
    ( Model
    , Msg(..)
    , init
    , saveKycData
    , update
    , view
    )

import Form
import Form.Select
import Form.Text
import Form.Validate
import Graphql.Http
import Html exposing (Html, text)
import Html.Attributes exposing (class, maxlength)
import Kyc exposing (ProfileKyc)
import Kyc.CostaRica.CedulaDeIdentidad as CedulaDeIdentidad
import Kyc.CostaRica.Dimex as Dimex
import Kyc.CostaRica.Nite as Nite
import Kyc.CostaRica.Phone as Phone
import Profile
import RemoteData exposing (RemoteData)
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared, Translators)
import UpdateResult as UR
import View.Feedback


type Msg
    = GotFormMsg (Form.Msg FormInput)
    | Submitted FormOutput
    | Saved (RemoteData (Graphql.Http.Error (Maybe ProfileKyc)) (Maybe ProfileKyc))


type CostaRicaDoc
    = CedulaDoc
    | DimexDoc
    | NiteDoc


type alias Model =
    { form : Form.Model FormInput }


type alias FormInput =
    { documentType : CostaRicaDoc
    , document : String
    , phoneNumber : String
    }


type alias FormOutput =
    { documentType : CostaRicaDoc
    , document : String
    , phoneNumber : String
    }


createForm : Translators -> Form.Form msg FormInput FormOutput
createForm ({ t } as translators) =
    Form.succeed FormOutput
        |> Form.with
            (Form.Select.init
                { label = t "register.form.document.type"
                , id = "document-type-select"
                , optionToString = docToString
                }
                |> Form.Select.withOption CedulaDoc (t "register.form.document.cedula_de_identidad.label")
                |> Form.Select.withOption DimexDoc (t "register.form.document.dimex.label")
                |> Form.Select.withOption NiteDoc (t "register.form.document.nite.label")
                |> Form.Select.withContainerAttrs [ class "mb-10" ]
                |> Form.select (docFromString >> Maybe.withDefault CedulaDoc)
                    { parser = Ok
                    , value = .documentType
                    , update = \documentType input -> { input | documentType = documentType }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            ((\{ documentType } ->
                let
                    { label, placeholder, maxLength, isValid } =
                        case documentType of
                            CedulaDoc ->
                                { label = t "register.form.document.cedula_de_identidad.label"
                                , placeholder = t "register.form.document.cedula_de_identidad.placeholder"
                                , maxLength = 9
                                , isValid = CedulaDeIdentidad.isValid
                                }

                            DimexDoc ->
                                { label = t "register.form.document.dimex.label"
                                , placeholder = t "register.form.document.dimex.placeholder"
                                , maxLength = 12
                                , isValid = Dimex.isValid
                                }

                            NiteDoc ->
                                { label = t "register.form.document.nite.label"
                                , placeholder = t "register.form.document.nite.placeholder"
                                , maxLength = 10
                                , isValid = Nite.isValid
                                }
                in
                Form.Text.init
                    { label = label
                    , id = "document.input"
                    }
                    |> Form.Text.withPlaceholder placeholder
                    |> Form.Text.asNumeric
                    |> Form.Text.withExtraAttrs [ maxlength maxLength ]
                    |> Form.textField
                        { parser =
                            Form.Validate.succeed
                                >> Form.Validate.stringLongerThan 0
                                >> Form.Validate.custom
                                    (\stringInput ->
                                        if isValid stringInput then
                                            Ok stringInput

                                        else
                                            Err (\translators_ -> translators_.t "register.form.document.errorInvalid")
                                    )
                                >> Form.Validate.validate translators
                        , value = .document
                        , update = \document input -> { input | document = String.filter Char.isDigit document }
                        , externalError = always Nothing
                        }
             )
                |> Form.introspect
            )
        |> Form.with
            (Form.Text.init
                { label = t "register.form.phone.label"
                , id = "phone_number_field"
                }
                |> Form.Text.withPlaceholder (t "register.form.phone.placeholder")
                |> Form.Text.withType Form.Text.Telephone
                |> Form.Text.asNumeric
                |> Form.Text.withExtraAttrs [ maxlength 8 ]
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.stringLengthExactly 8
                            >> Form.Validate.custom
                                (\stringPhone ->
                                    if Phone.isValid stringPhone then
                                        Ok stringPhone

                                    else
                                        Err
                                            (\translators_ ->
                                                translators_.t "error.phone"
                                            )
                                )
                            >> Form.Validate.validate translators
                    , value = .phoneNumber
                    , update = \phoneNumber input -> { input | phoneNumber = String.filter Char.isDigit phoneNumber }
                    , externalError = always Nothing
                    }
            )


init : Model
init =
    { form =
        Form.init
            { documentType = CedulaDoc
            , document = ""
            , phoneNumber = ""
            }
    }


docToString : CostaRicaDoc -> String
docToString docType =
    case docType of
        DimexDoc ->
            "DIMEX"

        NiteDoc ->
            "NITE"

        CedulaDoc ->
            "cedula_de_identidad"


docFromString : String -> Maybe CostaRicaDoc
docFromString docType =
    case docType of
        "DIMEX" ->
            Just DimexDoc

        "NITE" ->
            Just NiteDoc

        "cedula_de_identidad" ->
            Just CedulaDoc

        _ ->
            Nothing


view : Translators -> Model -> Html Msg
view ({ t } as translators) model =
    Form.view [ class "md:max-w-sm md:mx-auto py-6" ]
        translators
        (\submitButton ->
            [ submitButton [ class "button button-primary w-full" ]
                [ text <| t "profile.edit.submit" ]
            ]
        )
        (createForm translators)
        model.form
        { toMsg = GotFormMsg
        , onSubmit = Submitted
        }


type alias UpdateResult =
    UR.UpdateResult Model Msg View.Feedback.Model


update : Shared -> Model -> Msg -> UpdateResult
update shared model msg =
    case msg of
        GotFormMsg subMsg ->
            Form.update shared subMsg model.form
                |> UR.fromChild (\newForm -> { model | form = newForm })
                    GotFormMsg
                    UR.addExt
                    model

        Submitted _ ->
            model |> UR.init

        Saved _ ->
            model |> UR.init


saveKycData : LoggedIn.Model -> FormOutput -> LoggedIn.External Msg
saveKycData loggedIn formOutput =
    let
        data =
            { documentType =
                formOutput.documentType
                    |> docToString
                    |> String.toLower
            , document = formOutput.document
            , userType = "natural"
            , phone = formOutput.phoneNumber
            , isVerified = False
            }
    in
    LoggedIn.mutation loggedIn
        (Profile.upsertKycMutation data)
        Saved

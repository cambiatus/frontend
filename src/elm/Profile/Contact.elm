module Profile.Contact exposing
    ( Model
    , Msg
    , Normalized
    , decode
    , encode
    , hasSameType
    , init
    , selectionSet
    , unwrap
    , update
    , view
    )

import Api.Graphql
import Cambiatus.Enum.ContactType as ContactType exposing (ContactType(..))
import Cambiatus.Mutation
import Cambiatus.Object
import Cambiatus.Object.Contact
import Cambiatus.Object.User as User
import Eos.Account as Eos
import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, button, div, img, p, text)
import Html.Attributes exposing (class, classList, disabled, src, style, type_)
import Html.Events exposing (onClick, onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Extra as LE
import Regex exposing (Regex)
import RemoteData exposing (RemoteData)
import Session.Shared exposing (Shared, Translators)
import Validate
import View.Components
import View.Form
import View.Form.Input as Input
import View.Form.Select as Select



-- MODEL


type alias Model =
    { state : UpdatedData
    , kind : Kind
    }


init : Bool -> Model
init showTypeSelector =
    { state = RemoteData.NotAsked
    , kind =
        if showTypeSelector then
            Single (initBasic defaultContactType)

        else
            Multiple (List.map initBasic ContactType.list)
    }



-- TYPES


type Kind
    = Single Basic
    | Multiple (List Basic)


type alias Basic =
    { country : Country
    , contactType : ContactType
    , contact : String
    , errors : Maybe (List String)
    , showFlags : Bool
    }


initBasic : ContactType -> Basic
initBasic contactType =
    { country = Brazil
    , contactType = contactType
    , contact = ""
    , errors = Nothing
    , showFlags = False
    }


type alias Contact =
    { contactType : ContactType
    , contact : String
    }


defaultContactType : ContactType
defaultContactType =
    Whatsapp


type alias Profile =
    { account : Eos.Name
    , contacts : Maybe (List Normalized)
    }


{-| The contact string must be formatted in a specific way to be sent to the
backend, so we have this type to ensure the information is normalized
-}
type Normalized
    = Normalized Contact


type Country
    = Brazil
    | CostaRica
    | Ethiopia
    | UnitedStates


type alias UpdatedData =
    RemoteData (Graphql.Http.Error (Maybe Profile)) (Maybe Profile)



-- UPDATE


type Msg
    = SelectedCountry ContactType Country
    | ClickedToggleContactFlags ContactType
    | EnteredContactText ContactType String
    | EnteredContactOption String
    | ClickedSubmit
    | CompletedUpdateContact UpdatedData


update : Msg -> Model -> Shared -> Eos.Name -> ( Model, Cmd Msg, Maybe (List Normalized) )
update msg model ({ translators } as shared) accountName =
    let
        toggleFlags ({ showFlags } as contact) =
            { contact | showFlags = not showFlags }

        setCountry newCountry contact =
            { contact | country = newCountry, errors = Nothing }

        setContact newContact contact =
            { contact | contact = newContact }

        updateKind contactType updateFn =
            { model
                | kind =
                    case model.kind of
                        Single contact ->
                            updateFn contact
                                |> Single

                        Multiple contacts ->
                            updateIfContactType contactType updateFn contacts
                                |> Multiple
            }
    in
    case msg of
        SelectedCountry contactType country ->
            ( updateKind contactType (setCountry country)
            , Cmd.none
            , Nothing
            )

        ClickedToggleContactFlags contactType ->
            ( updateKind contactType toggleFlags
            , Cmd.none
            , Nothing
            )

        EnteredContactText contactType newContact ->
            ( updateKind contactType (setContact newContact)
            , Cmd.none
            , Nothing
            )

        EnteredContactOption option ->
            case model.kind of
                Single contact ->
                    ( { model
                        | kind =
                            Single
                                { contact
                                    | contactType =
                                        ContactType.fromString option
                                            |> Maybe.withDefault defaultContactType
                                    , errors = Nothing
                                    , contact = ""
                                }
                      }
                    , Cmd.none
                    , Nothing
                    )

                Multiple _ ->
                    ( model
                    , Cmd.none
                    , Nothing
                    )

        ClickedSubmit ->
            case submit translators model.kind of
                Err withError ->
                    ( { model | kind = withError }
                    , Cmd.none
                    , Nothing
                    )

                Ok normalized ->
                    ( { model | state = RemoteData.Loading }
                    , Api.Graphql.mutation shared
                        (mutation accountName normalized)
                        (RemoteData.fromResult >> CompletedUpdateContact)
                    , Nothing
                    )

        CompletedUpdateContact result ->
            ( { model | state = result }
            , Cmd.none
            , RemoteData.toMaybe result
                |> Maybe.andThen (Maybe.andThen .contacts)
            )


updateIfContactType : ContactType -> (Basic -> Basic) -> List Basic -> List Basic
updateIfContactType contactType fn contacts =
    LE.updateIf (.contactType >> (==) contactType) fn contacts


submit : Translators -> Kind -> Result Kind (List Normalized)
submit translators model =
    case model of
        Single contact ->
            submitSingle translators contact
                |> Result.mapError Single
                |> Result.map List.singleton

        Multiple contacts ->
            submitMultiple translators contacts
                |> Result.mapError Multiple


submitSingle : Translators -> Basic -> Result Basic Normalized
submitSingle translators basic =
    basic
        |> Validate.validate (validator basic.contactType translators)
        |> Result.mapError (\errors -> addErrors errors basic)
        |> Result.map (\valid -> normalize basic.country valid)


submitMultiple : Translators -> List Basic -> Result (List Basic) (List Normalized)
submitMultiple translators basics =
    let
        ( withError, valid ) =
            List.foldr
                (\basic ( basicsAcc, normalizedsAcc ) ->
                    case submitSingle translators basic of
                        Err basicWithErrors ->
                            ( basicWithErrors :: basicsAcc, normalizedsAcc )

                        Ok normalized ->
                            ( { basic | errors = Nothing } :: basicsAcc
                            , normalized :: normalizedsAcc
                            )
                )
                ( [], [] )
                basics
    in
    if List.length valid == List.length basics then
        Ok valid

    else
        Err withError



-- VIEW


view : Translators -> Model -> Html Msg
view translators model =
    let
        submitText =
            (case model.kind of
                Single contact ->
                    if usesPhone contact.contactType then
                        "contact_form.phone.submit"

                    else
                        "contact_form.username.submit"

                Multiple _ ->
                    "contact_form.submit_multiple"
            )
                |> translators.t
                |> text

        isDisabled =
            RemoteData.isLoading model.state || RemoteData.isSuccess model.state

        submitButton =
            button
                [ class "button w-full"
                , type_ "submit"
                , classList
                    [ ( "button-success", RemoteData.isSuccess model.state )
                    , ( "button-primary", not (RemoteData.isSuccess model.state) )
                    ]
                , disabled isDisabled
                ]
                [ case model.state of
                    RemoteData.Loading ->
                        View.Components.loadingLogoAnimatedFluid

                    _ ->
                        submitText
                ]
    in
    (case model.kind of
        Single contact ->
            [ viewContactTypeSelect translators contact.contactType
            , viewInput translators contact
            ]

        Multiple contacts ->
            List.map (viewInput translators) contacts
    )
        ++ [ submitButton
           , if RemoteData.isFailure model.state then
                text (translators.t "contact_form.error")

             else
                text ""
           ]
        |> Html.form
            [ class "w-full md:w-5/6 mx-auto mt-12"
            , onSubmit ClickedSubmit
            ]


viewInput : Translators -> Basic -> Html Msg
viewInput translators basic =
    div [ class "flex space-x-4" ]
        (if usesPhone basic.contactType then
            [ viewFlagsSelect translators basic, viewPhoneInput translators basic ]

         else
            [ viewProfileInput translators basic ]
        )


viewContactTypeSelect : Translators -> ContactType -> Html Msg
viewContactTypeSelect translators contactType =
    let
        contactOptions =
            List.map
                (\contactType_ ->
                    let
                        asString =
                            ContactType.toString contactType_

                        capitalized =
                            (String.left 1 asString |> String.toUpper)
                                ++ String.dropLeft 1 (String.toLower asString)
                    in
                    { value = asString
                    , label = capitalized
                    }
                )
                ContactType.list
    in
    List.foldr Select.withOption
        (Select.init "contact_type"
            (translators.t "contact_form.contact_type")
            EnteredContactOption
            (ContactType.toString contactType)
            Nothing
        )
        contactOptions
        |> Select.toHtml


viewFlagsSelect : Translators -> Basic -> Html Msg
viewFlagsSelect { t } basic =
    let
        countryOptions =
            basic.country
                :: List.filter (\country -> country /= basic.country) listCountries

        -- For example, in Brazil we only use +55, not +055
        readableCountryCode country =
            case countryCode country |> String.toList of
                '+' :: rest ->
                    "+" ++ (LE.dropWhile ((==) '0') rest |> String.fromList)

                code ->
                    String.fromList code

        flag classes country =
            button
                [ class ("w-full flex items-center space-x-2 text-menu " ++ classes)
                , onClick (SelectedCountry basic.contactType country)
                , type_ "button"
                ]
                [ img
                    [ class "w-7"
                    , src (countryToFlag country)
                    ]
                    []
                , p [ class "justify-self-center text-left", style "min-width" "4ch" ]
                    [ text (readableCountryCode country) ]
                ]

        id =
            ContactType.toString basic.contactType ++ "_select"
    in
    div [ class "mb-10 flex-shrink-0", Html.Attributes.id id ]
        [ if basic.showFlags then
            button
                [ class "absolute top-0 left-0 w-full h-full cursor-default z-40"
                , onClick (ClickedToggleContactFlags basic.contactType)
                , type_ "button"
                ]
                []

          else
            text ""
        , View.Form.label id (t "contact_form.country")
        , button
            [ class "form-select select relative"
            , classList [ ( "border-none mx-px", basic.showFlags ) ]
            , onClick (ClickedToggleContactFlags basic.contactType)
            , type_ "button"
            ]
            [ flag "" basic.country
            , if basic.showFlags then
                div
                    [ class "absolute form-input -mx-px inset-x-0 top-0 space-y-4 z-50" ]
                    (List.map (flag "mt-px") countryOptions)

              else
                text ""
            ]
        ]


viewPhoneInput : Translators -> Basic -> Html Msg
viewPhoneInput ({ t } as translators) basic =
    div [ class "w-full" ]
        [ Input.init
            { label = t "contact_form.phone.label"
            , id = ContactType.toString basic.contactType ++ "_input"
            , onInput = EnteredContactText basic.contactType
            , disabled = False
            , value = basic.contact
            , placeholder = Just (t "contact_form.phone.placeholder")
            , problems = basic.errors
            , translators = translators
            }
            |> Input.toHtml
        ]


viewProfileInput : Translators -> Basic -> Html Msg
viewProfileInput ({ t } as translators) basic =
    div [ class "w-full" ]
        [ Input.init
            { label = t "contact_form.username.label"
            , id = ContactType.toString basic.contactType ++ "_input"
            , onInput = EnteredContactText basic.contactType
            , disabled = False
            , value = basic.contact
            , placeholder = Just (t "contact_form.username.placeholder")
            , problems = basic.errors
            , translators = translators
            }
            |> Input.toHtml
        ]



-- UTILITIES


usesPhone : ContactType -> Bool
usesPhone contactType =
    List.member contactType [ Whatsapp, Phone ]


unwrap : Normalized -> Contact
unwrap (Normalized contact) =
    contact


hasSameType : Normalized -> Normalized -> Bool
hasSameType (Normalized contact1) (Normalized contact2) =
    contact1.contactType == contact2.contactType


addErrors : List String -> Basic -> Basic
addErrors errors basic =
    case errors of
        [] ->
            { basic | errors = Nothing }

        err :: _ ->
            { basic | errors = Just [ err ] }



-- NORMALIZING


countryCode : Country -> String
countryCode country =
    case country of
        Brazil ->
            "+055"

        CostaRica ->
            "+506"

        Ethiopia ->
            "+251"

        UnitedStates ->
            "+001"


normalize : Country -> Validate.Valid Basic -> Normalized
normalize country validatedContact =
    let
        { contactType, contact } =
            Validate.fromValid validatedContact
    in
    Normalized
        { contactType = contactType
        , contact =
            case contactType of
                Instagram ->
                    "https://instagram.com/" ++ contact

                Phone ->
                    String.join " " [ countryCode country, contact ]

                Telegram ->
                    "https://t.me/" ++ contact

                Whatsapp ->
                    String.join " " [ countryCode country, contact ]
        }



-- VALIDATING


phoneRegex : Regex
phoneRegex =
    Regex.fromString "^\\+?\\(?[0-9]{3}\\)?[-\\s\\.]?\\(?[0-9]{2}?\\)?[-\\s\\.]?[0-9]{3,5}[-\\s\\.]?[0-9]{3,4}$"
        |> Maybe.withDefault Regex.never


telegramRegex : Regex
telegramRegex =
    Regex.fromString "^[\\w]{5,32}$"
        |> Maybe.withDefault Regex.never


instagramRegex : Regex
instagramRegex =
    Regex.fromString "^[\\w](?!.*?\\.{2})[\\w.]{1,28}[\\w]$"
        |> Maybe.withDefault Regex.never


validateRegex : Regex -> String -> Validate.Validator String Basic
validateRegex regex error =
    Validate.fromErrors
        (\{ contact } ->
            if Regex.contains regex contact then
                []

            else
                [ error ]
        )


validator : ContactType -> Translators -> Validate.Validator String Basic
validator contactType translators =
    let
        ( regex, field ) =
            case contactType of
                Phone ->
                    ( phoneRegex, "phone" )

                Whatsapp ->
                    ( phoneRegex, "phone" )

                Instagram ->
                    ( instagramRegex, "username" )

                Telegram ->
                    ( telegramRegex, "username" )

        baseTranslation =
            "contact_form.validation"
    in
    Validate.all
        [ Validate.ifBlank .contact
            (translators.t (String.join "." [ baseTranslation, field, "blank" ]))
        , validateRegex regex
            (translators.t (String.join "." [ baseTranslation, field, "invalid" ]))
        ]



-- COUNTRY


listCountries : List Country
listCountries =
    [ Brazil, CostaRica, Ethiopia, UnitedStates ]


countryToFlag : Country -> String
countryToFlag country =
    case country of
        Brazil ->
            "/icons/flag-brazil.svg"

        CostaRica ->
            "/icons/flag-costa-rica.svg"

        Ethiopia ->
            "/icons/flag-ethiopia.svg"

        UnitedStates ->
            "/icons/flag-usa.svg"



-- JSON
-- All the data sent to/received from the backend should be normalized


decode : Decoder Normalized
decode =
    Decode.map2 (\contactType contact -> Normalized { contactType = contactType, contact = contact })
        ContactType.decoder
        Decode.string


encode : Normalized -> Encode.Value
encode (Normalized { contactType, contact }) =
    Encode.object
        [ ( "type", Encode.string (ContactType.toString contactType) )
        , ( "externalId", Encode.string contact )
        ]



-- GRAPHQL


selectionSet : SelectionSet (Maybe Normalized) Cambiatus.Object.Contact
selectionSet =
    SelectionSet.succeed
        (\maybeType maybeExternalId ->
            case ( maybeType, maybeExternalId ) of
                ( Just type_, Just externalId ) ->
                    Contact type_ externalId |> Normalized |> Just

                _ ->
                    Nothing
        )
        |> with Cambiatus.Object.Contact.type_
        |> with Cambiatus.Object.Contact.externalId


profileSelectionSet : SelectionSet Profile Cambiatus.Object.User
profileSelectionSet =
    SelectionSet.succeed Profile
        |> with (Eos.nameSelectionSet User.account)
        |> with
            (User.contacts selectionSet
                |> SelectionSet.map (Maybe.map (List.filterMap identity))
            )


mutation : Eos.Name -> List Normalized -> SelectionSet (Maybe Profile) RootMutation
mutation account contacts =
    let
        contactInput (Normalized { contactType, contact }) =
            { type_ = Present contactType, externalId = Present contact }
    in
    Cambiatus.Mutation.updateUser
        { input =
            { account = Eos.nameToString account
            , avatar = Absent
            , bio = Absent
            , contacts = Present (List.map contactInput contacts)
            , email = Absent
            , interests = Absent
            , location = Absent
            , name = Absent
            }
        }
        profileSelectionSet

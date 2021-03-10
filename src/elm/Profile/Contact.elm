module Profile.Contact exposing
    ( ContactResponse(..)
    , Model
    , Msg
    , Normalized
    , decode
    , encode
    , hasSameType
    , initMultiple
    , initSingle
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
import Icons
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Extra as LE
import PhoneNumber exposing (Country)
import PhoneNumber.Countries as Countries
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


initSingle : Model
initSingle =
    { state = RemoteData.NotAsked
    , kind = Single (initBasic defaultContactType)
    }


initMultiple : List Normalized -> Model
initMultiple initialContacts =
    { state = RemoteData.NotAsked
    , kind =
        Multiple
            (List.map
                (\type_ ->
                    initialContacts
                        |> LE.find (\(Normalized { contactType }) -> type_ == contactType)
                        |> Maybe.map initBasicWith
                        |> Maybe.withDefault (initBasic type_)
                )
                ContactType.list
            )
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
    { country = Countries.countryBR
    , contactType = contactType
    , contact = ""
    , errors = Nothing
    , showFlags = False
    }


initBasicWith : Normalized -> Basic
initBasicWith ((Normalized { contactType }) as normalized) =
    let
        initial =
            initBasic contactType

        ( maybeCountry, newContact ) =
            countryFromNormalized normalized
    in
    { initial
        | contact = newContact
        , country =
            maybeCountry
                |> Maybe.withDefault Countries.countryBR
    }


countryFromNormalized : Normalized -> ( Maybe Country, String )
countryFromNormalized (Normalized { contactType, contact }) =
    if usesPhone contactType then
        let
            countryCode =
                String.dropLeft 1 contact
                    |> String.toList
                    |> LE.takeWhile ((/=) ' ')
                    |> String.fromList

            country =
                List.filter (.countryCode >> (==) countryCode) supportedCountries
                    |> List.head

            newContact =
                Maybe.map (.countryCode >> String.length >> (+) 1) country
                    |> Maybe.withDefault 0
                    |> (\n -> String.dropLeft n contact |> String.trim)
        in
        ( country, newContact )

    else
        ( Nothing, contact )


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


type alias UpdatedData =
    RemoteData (Graphql.Http.Error (Maybe Profile)) (Maybe Profile)


type ContactResponse
    = NotAsked
    | WithContacts String (List Normalized)
    | WithError String


contactResponseFromMaybe : Translators -> String -> Maybe (List Normalized) -> ContactResponse
contactResponseFromMaybe { t } baseTranslation maybeContacts =
    maybeContacts
        |> Maybe.map (WithContacts (t <| baseTranslation ++ "success"))
        |> Maybe.withDefault (WithError (t <| baseTranslation ++ "failure"))



-- UPDATE


type Msg
    = SelectedCountry ContactType Country
    | ClickedToggleContactFlags ContactType
    | EnteredContactText ContactType String
    | EnteredContactOption String
    | ClickedSubmit
    | CompletedUpdateContact UpdatedData


update : Msg -> Model -> Shared -> String -> ( Model, Cmd Msg, ContactResponse )
update msg model ({ translators } as shared) authToken =
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
            , NotAsked
            )

        ClickedToggleContactFlags contactType ->
            ( updateKind contactType toggleFlags
            , Cmd.none
            , NotAsked
            )

        EnteredContactText contactType newContact ->
            ( updateKind contactType (setContact newContact)
            , Cmd.none
            , NotAsked
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
                    , NotAsked
                    )

                Multiple _ ->
                    ( model
                    , Cmd.none
                    , NotAsked
                    )

        ClickedSubmit ->
            case submit translators model.kind of
                Err withError ->
                    ( { model | kind = withError }
                    , Cmd.none
                    , NotAsked
                    )

                Ok normalized ->
                    ( { model
                        | state = RemoteData.Loading
                        , kind = removeErrors model.kind
                      }
                    , Api.Graphql.mutation shared
                        (Just authToken)
                        (mutation normalized)
                        CompletedUpdateContact
                    , NotAsked
                    )

        CompletedUpdateContact result ->
            let
                feedbackScope =
                    case model.kind of
                        Single _ ->
                            "single"

                        Multiple _ ->
                            "multiple"
            in
            ( { model | state = result }
            , Cmd.none
            , RemoteData.toMaybe result
                |> Maybe.andThen (Maybe.andThen .contacts)
                |> contactResponseFromMaybe translators ("contact_form.feedback." ++ feedbackScope ++ ".")
            )


updateIfContactType : ContactType -> (Basic -> Basic) -> List Basic -> List Basic
updateIfContactType contactType fn contacts =
    LE.updateIf (.contactType >> (==) contactType) fn contacts


submit : Translators -> Kind -> Result Kind (List Normalized)
submit translators kind =
    case kind of
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
    if List.length valid > 0 then
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
                    , ( "mt-7", isMultiple model.kind )
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
            List.map (viewInputWithBackground translators) contacts
    )
        ++ [ submitButton
           , if RemoteData.isFailure model.state then
                text (translators.t "contact_form.error")

             else
                text ""
           ]
        |> Html.form
            [ class "w-full md:w-5/6 mx-auto mt-12"
            , classList [ ( "px-4 lg:w-2/3", isMultiple model.kind ) ]
            , onSubmit ClickedSubmit
            ]


viewInputWithBackground : Translators -> Basic -> Html Msg
viewInputWithBackground translators basic =
    div [ class "bg-gray-100 p-4 pb-0 rounded mb-4" ]
        [ div [ class "font-menu font-medium flex items-center mb-4" ]
            [ contactTypeToIcon "mr-2" basic.contactType
            , text (contactTypeToString translators basic.contactType)
            ]
        , viewInput translators basic
        ]


contactTypeToString : Translators -> ContactType -> String
contactTypeToString translators contactType =
    ContactType.toString contactType
        |> String.toLower
        |> (\asString -> "contact_form.types." ++ asString)
        |> translators.t


contactTypeToIcon : String -> ContactType -> Html msg
contactTypeToIcon class_ contactType =
    case contactType of
        Phone ->
            Icons.phone class_

        Instagram ->
            Icons.instagram class_

        Telegram ->
            Icons.telegram class_

        Whatsapp ->
            Icons.whatsapp class_


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
                            contactTypeToString translators contactType_
                    in
                    { value = ContactType.toString contactType_
                    , label = asString
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
                :: List.filter (\country -> country /= basic.country) supportedCountries

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
                    [ text ("+" ++ country.countryCode) ]
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
viewPhoneInput ({ t, tr } as translators) basic =
    div [ class "w-full" ]
        [ Input.init
            { label = t "contact_form.phone.label"
            , id = ContactType.toString basic.contactType ++ "_input"
            , onInput = EnteredContactText basic.contactType
            , disabled = False
            , value = basic.contact
            , placeholder =
                Just
                    (tr "contact_form.phone.placeholder"
                        [ ( "example_number", phonePlaceholder basic.country ) ]
                    )
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


removeErrors : Kind -> Kind
removeErrors kind =
    case kind of
        Single contact ->
            Single { contact | errors = Nothing }

        Multiple contacts ->
            List.map (\contact -> { contact | errors = Nothing }) contacts
                |> Multiple


isMultiple : Kind -> Bool
isMultiple kind =
    case kind of
        Multiple _ ->
            True

        Single _ ->
            True



-- NORMALIZING


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
                    String.join " " [ "+" ++ country.countryCode, contact ]

                Telegram ->
                    "https://t.me/" ++ contact

                Whatsapp ->
                    String.join " " [ "+" ++ country.countryCode, contact ]
        }



-- VALIDATING


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


validatePhone : String -> Validate.Validator String Basic
validatePhone error =
    Validate.fromErrors
        (\{ country, contact } ->
            if
                PhoneNumber.valid
                    { defaultCountry = country
                    , otherCountries = []
                    , types = PhoneNumber.anyType
                    }
                    contact
            then
                []

            else
                [ error ]
        )


validator : ContactType -> Translators -> Validate.Validator String Basic
validator contactType translators =
    let
        ( specificValidation, field ) =
            case contactType of
                Phone ->
                    ( validatePhone, "phone" )

                Whatsapp ->
                    ( validatePhone, "phone" )

                Instagram ->
                    ( validateRegex instagramRegex, "username" )

                Telegram ->
                    ( validateRegex telegramRegex, "username" )

        baseTranslation =
            "contact_form.validation"
    in
    Validate.all
        [ Validate.ifBlank .contact
            (translators.t (String.join "." [ baseTranslation, field, "blank" ]))
        , specificValidation
            (translators.t (String.join "." [ baseTranslation, field, "invalid" ]))
        ]



-- COUNTRY


supportedCountries : List Country
supportedCountries =
    [ Countries.countryBR
    , Countries.countryCR
    , Countries.countryET
    , Countries.countryUS
    ]


countryToFlag : Country -> String
countryToFlag country =
    if country == Countries.countryBR then
        "/icons/flag-brazil.svg"

    else if country == Countries.countryCR then
        "/icons/flag-costa-rica.svg"

    else if country == Countries.countryET then
        "/icons/flag-ethiopia.svg"

    else if country == Countries.countryUS then
        "/icons/flag-usa.svg"

    else
        ""


phonePlaceholder : Country -> String
phonePlaceholder country =
    if country == Countries.countryBR then
        "11 91234 5678"

    else if country == Countries.countryCR then
        "8123 4567"

    else if country == Countries.countryET then
        "91 234 5678"

    else if country == Countries.countryUS then
        "209 123 4567"

    else
        ""



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


mutation : List Normalized -> SelectionSet (Maybe Profile) RootMutation
mutation contacts =
    let
        contactInput (Normalized { contactType, contact }) =
            { type_ = Present contactType, externalId = Present contact }
    in
    Cambiatus.Mutation.updateUser
        { input =
            { avatar = Absent
            , bio = Absent
            , contacts = Present (List.map contactInput contacts)
            , email = Absent
            , interests = Absent
            , location = Absent
            , name = Absent
            }
        }
        profileSelectionSet

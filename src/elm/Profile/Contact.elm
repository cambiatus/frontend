module Profile.Contact exposing
    ( Country(..)
    , Model
    , Msg
    , Normalized
    , addErrors
    , decode
    , defaultContactType
    , encode
    , hasSameType
    , init
    , normalize
    , selectionSet
    , unwrap
    , update
    , updateType
    , usesPhone
    , validator
    , viewForm
    )

import Cambiatus.Enum.ContactType as ContactType exposing (ContactType)
import Cambiatus.Object
import Cambiatus.Object.Contact
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, button, div, img, p, text)
import Html.Attributes exposing (class, classList, src, style, type_)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Extra
import Regex exposing (Regex)
import Session.Shared exposing (Translators)
import Validate
import View.Form
import View.Form.Input as Input



-- MODEL


type alias Model =
    { contactType : ContactType
    , id : String
    , country : Country
    , contact : String
    , errors : Maybe (List String)
    , showFlags : Bool
    }


init : String -> Model
init id =
    { contactType = defaultContactType
    , id = id
    , country = Brazil
    , contact = ""
    , errors = Nothing
    , showFlags = False
    }


defaultContactType : ContactType
defaultContactType =
    ContactType.Whatsapp



-- TYPES


type alias Contact =
    { contactType : ContactType
    , contact : String
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



-- UPDATE


type Msg
    = SelectedCountry Country
    | ClickedToggleContactFlags
    | EnteredContactText String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectedCountry country ->
            { model | country = country }

        ClickedToggleContactFlags ->
            { model | showFlags = not model.showFlags }

        EnteredContactText contact ->
            { model | contact = contact }



-- VIEW


viewForm : Translators -> Model -> Html Msg
viewForm translators model =
    div [ class "flex space-x-4" ]
        (if usesPhone model.contactType then
            [ viewFlagsSelect translators model, viewPhoneInput translators model ]

         else
            [ viewProfileInput translators model ]
        )


viewFlagsSelect : Translators -> Model -> Html Msg
viewFlagsSelect { t } model =
    let
        countryOptions =
            model.country
                :: List.filter (\country -> country /= model.country) listCountries

        -- For example, in Brazil we only use +55, not +055
        readableCountryCode country =
            case countryCode country |> String.toList of
                '+' :: rest ->
                    "+" ++ (List.Extra.dropWhile ((==) '0') rest |> String.fromList)

                code ->
                    String.fromList code

        flag classes country =
            button
                [ class ("w-full flex items-center space-x-2 text-menu " ++ classes)
                , onClick (SelectedCountry country)
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
            model.id ++ "_select"
    in
    div [ class "mb-10 flex-shrink-0", Html.Attributes.id id ]
        [ if model.showFlags then
            button
                [ class "absolute top-0 left-0 w-full h-full cursor-default z-40"
                , onClick ClickedToggleContactFlags
                , type_ "button"
                ]
                []

          else
            text ""
        , View.Form.label id (t "contact_form.country")
        , button
            [ class "form-select select relative"
            , classList [ ( "border-none mx-px", model.showFlags ) ]
            , onClick ClickedToggleContactFlags
            , type_ "button"
            ]
            [ flag "" model.country
            , if model.showFlags then
                div
                    [ class "absolute form-input -mx-px inset-x-0 top-0 space-y-4 z-50" ]
                    (List.map (flag "mt-px") countryOptions)

              else
                text ""
            ]
        ]


viewPhoneInput : Translators -> Model -> Html Msg
viewPhoneInput ({ t } as translators) model =
    div [ class "w-full" ]
        [ Input.init
            { label = t "contact_form.phone.label"
            , id = model.id ++ "_contact_input"
            , onInput = EnteredContactText
            , disabled = False
            , value = model.contact
            , placeholder = Just (t "contact_form.phone.placeholder")
            , problems = model.errors
            , translators = translators
            }
            |> Input.toHtml
        ]


viewProfileInput : Translators -> Model -> Html Msg
viewProfileInput ({ t } as translators) model =
    div [ class "w-full" ]
        [ Input.init
            { label = t "contact_form.username.label"
            , id = model.id ++ "_contact_input"
            , onInput = EnteredContactText
            , disabled = False
            , value = model.contact
            , placeholder = Just (t "contact_form.username.placeholder")
            , problems = model.errors
            , translators = translators
            }
            |> Input.toHtml
        ]



-- UTILITIES


usesPhone : ContactType -> Bool
usesPhone contactType =
    List.member contactType [ ContactType.Whatsapp, ContactType.Phone ]


unwrap : Normalized -> Contact
unwrap (Normalized contact) =
    contact


hasSameType : Normalized -> Normalized -> Bool
hasSameType (Normalized contact1) (Normalized contact2) =
    contact1.contactType == contact2.contactType


updateType : ContactType -> Model -> Model
updateType newType model =
    { model | contactType = newType, errors = Nothing }


addErrors : List String -> Model -> Model
addErrors errors model =
    case errors of
        [] ->
            { model | errors = Nothing }

        err :: _ ->
            { model | errors = Just [ err ] }



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


normalize : Country -> Validate.Valid Model -> Normalized
normalize country validatedContact =
    let
        { contactType, contact } =
            Validate.fromValid validatedContact
    in
    Normalized
        { contactType = contactType
        , contact =
            case contactType of
                ContactType.Instagram ->
                    "https://instagram.com/" ++ contact

                ContactType.Phone ->
                    String.join " " [ countryCode country, contact ]

                ContactType.Telegram ->
                    "https://t.me/" ++ contact

                ContactType.Whatsapp ->
                    String.join " " [ countryCode country, contact ]
        }



-- VALIDATING


phoneRegex : Regex
phoneRegex =
    Regex.fromString "^\\+?\\(?[0-9]{3}\\)?[-\\s\\.]?\\(?[0-9]{2}?\\)?[-\\s\\.]?[0-9]{3,5}[-\\s\\.]?[0-9]{3,4}$"
        |> Maybe.withDefault Regex.never


validator : ContactType -> Translators -> Validate.Validator String Model
validator contactType translators =
    let
        phoneValidations =
            [ Validate.ifBlank .contact (translators.t "contact_validator.phone.blank")
            , Validate.fromErrors
                (\{ contact } ->
                    if Regex.contains phoneRegex contact then
                        []

                    else
                        [ translators.t "contact_validator.phone.invalid" ]
                )
            ]

        profileValidations =
            [ Validate.ifBlank .contact (translators.t "contact_validator.profile.blank")
            ]
    in
    Validate.all
        (if usesPhone contactType then
            phoneValidations

         else
            profileValidations
        )



-- COUNTRY


listCountries : List Country
listCountries =
    [ Brazil, CostaRica, Ethiopia ]


countryToFlag : Country -> String
countryToFlag country =
    case country of
        Brazil ->
            "/icons/flag-brazil.svg"

        CostaRica ->
            "/icons/flag-costa-rica.svg"

        Ethiopia ->
            "/icons/flag-ethiopia.svg"



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

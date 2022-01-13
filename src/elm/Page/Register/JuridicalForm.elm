module Page.Register.JuridicalForm exposing (FormInput, FormOutput, companyTypeToString, decoder, encode, form, init)

import Address
import Cambiatus.Scalar exposing (Id(..))
import Eos.Account
import Form
import Form.Select
import Form.Text
import Form.Validate
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Json.Decode.Pipeline
import Json.Encode as Encode
import List.Extra
import Maybe.Extra
import Page.Register.Common as Common
import Session.Shared exposing (Translators)
import Set exposing (Set)


type alias FormInput =
    { companyType : CompanyType
    , document : String
    , name : String
    , email : String
    , phoneNumber : String
    , state : Maybe { id : String, name : String }
    , city : Maybe { id : String, name : String }
    , district : Maybe { id : String, name : String }
    , account : String
    , street : String
    , zip : String
    , number : String
    , country : Address.Country
    }


type alias FormOutput =
    { companyType : CompanyType
    , document : String
    , name : String
    , email : String
    , phone : String
    , account : Eos.Account.Name
    , state : { id : String, name : String }
    , city : { id : String, name : String }
    , district : { id : String, name : String }
    , street : String
    , number : String
    , zip : String
    }


encode : FormOutput -> Encode.Value
encode output =
    let
        encodeIdAndName { id, name } =
            Encode.object
                [ ( "id", Encode.string id )
                , ( "name", Encode.string name )
                ]
    in
    Encode.object
        [ ( "companyType", Encode.string <| companyTypeToString output.companyType )
        , ( "document", Encode.string output.document )
        , ( "name", Encode.string output.name )
        , ( "email", Encode.string output.email )
        , ( "phone", Encode.string output.phone )
        , ( "account", Eos.Account.encodeName output.account )
        , ( "state", encodeIdAndName output.state )
        , ( "city", encodeIdAndName output.city )
        , ( "district", encodeIdAndName output.district )
        , ( "street", Encode.string output.street )
        , ( "number", Encode.string output.number )
        , ( "zip", Encode.string output.zip )
        ]


decoder : Decode.Decoder FormOutput
decoder =
    let
        decodeIdAndName =
            Decode.map2 (\id name -> { id = id, name = name })
                Decode.string
                Decode.string
    in
    Decode.succeed FormOutput
        |> Json.Decode.Pipeline.required "companyType"
            (Decode.string
                |> Decode.andThen
                    (\companyType ->
                        case companyTypeFromString companyType of
                            Nothing ->
                                Decode.fail "Invalid company type"

                            Just type_ ->
                                Decode.succeed type_
                    )
            )
        |> Json.Decode.Pipeline.required "document" Decode.string
        |> Json.Decode.Pipeline.required "name" Decode.string
        |> Json.Decode.Pipeline.required "email" Decode.string
        |> Json.Decode.Pipeline.required "phone" Decode.string
        |> Json.Decode.Pipeline.required "account" Eos.Account.nameDecoder
        |> Json.Decode.Pipeline.required "state" decodeIdAndName
        |> Json.Decode.Pipeline.required "city" decodeIdAndName
        |> Json.Decode.Pipeline.required "district" decodeIdAndName
        |> Json.Decode.Pipeline.required "street" Decode.string
        |> Json.Decode.Pipeline.required "number" Decode.string
        |> Json.Decode.Pipeline.required "zip" Decode.string


form : Translators -> { unavailableAccounts : Set String } -> Form.Form msg FormInput FormOutput
form ({ t } as translators) unavailableAccounts =
    let
        unwrapId (Id id) =
            id
    in
    Form.succeed FormOutput
        |> Form.with
            (Form.Select.init
                { label = t "register.form.company_type"
                , id = "company-type-select"
                , optionToString = companyTypeToString
                }
                |> Form.Select.withOption MIPYME (t "register.form.company.mipyme.label")
                |> Form.Select.withOption Corporation (t "register.form.company.gran_empresa.label")
                |> Form.Select.withContainerAttrs [ class "mb-10" ]
                |> Form.select (companyTypeFromString >> Maybe.withDefault MIPYME)
                    { parser = Ok
                    , value = .companyType
                    , update = \companyType input -> { input | companyType = companyType }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            ((\{ companyType } ->
                Form.Text.init
                    { label = t ("register.form.company." ++ companyTypeToString companyType ++ ".label")
                    , id = "document-input"
                    }
                    |> Form.Text.withPlaceholder (t ("register.form.company." ++ companyTypeToString companyType ++ ".placeholder"))
                    |> Form.Text.withCounter (Form.Text.CountLetters 10)
                    |> Form.textField
                        { parser = Ok
                        , value = .document
                        , update = \document input -> { input | document = document }
                        , externalError = always Nothing
                        }
             )
                |> Form.introspect
            )
        |> Form.with
            (Form.Text.init { label = t "register.form.company_name", id = "company-name-input" }
                |> Form.Text.withPlaceholder (t "register.form.name_example")
                |> Form.textField
                    { parser = Ok
                    , value = .name
                    , update = \name input -> { input | name = name }
                    , externalError = always Nothing
                    }
            )
        |> Form.with (Common.emailField translators)
        |> Form.with (Common.phoneNumberField translators)
        |> Form.with (Common.accountNameField translators unavailableAccounts)
        |> Form.with
            ((\{ country } ->
                Form.Select.init
                    { label = t "register.form.state"
                    , id = "state-select"
                    , optionToString =
                        Maybe.map nameAndIdToString
                            >> Maybe.withDefault ""
                    }
                    |> Form.Select.withOption Nothing (t "register.form.select.state")
                    |> Form.Select.withOptions
                        (country.states
                            |> List.map
                                (\state ->
                                    { option =
                                        Just
                                            { id = unwrapId state.id
                                            , name = state.name
                                            }
                                    , label = state.name
                                    }
                                )
                        )
                    |> Form.Select.withContainerAttrs [ class "mb-10" ]
                    |> Form.select nameAndIdFromString
                        { parser =
                            Form.Validate.succeed
                                >> Form.Validate.required
                                >> Form.Validate.validate translators
                        , value = .state
                        , update = \state input -> { input | state = state }
                        , externalError = always Nothing
                        }
             )
                |> Form.introspect
            )
        |> Form.with
            ((\{ state, country } ->
                case state of
                    Nothing ->
                        Form.fail

                    Just validState ->
                        case List.Extra.find (\{ id } -> unwrapId id == validState.id) country.states of
                            Nothing ->
                                Form.fail

                            Just stateFromCountry ->
                                Form.Select.init
                                    { label = t "register.form.city"
                                    , id = "city-select"
                                    , optionToString =
                                        Maybe.map nameAndIdToString
                                            >> Maybe.withDefault ""
                                    }
                                    |> Form.Select.withOption Nothing (t "register.form.select.city")
                                    |> Form.Select.withOptions
                                        (List.map
                                            (\city ->
                                                { option = Just { id = unwrapId city.id, name = city.name }
                                                , label = city.name
                                                }
                                            )
                                            stateFromCountry.cities
                                        )
                                    |> Form.Select.withContainerAttrs [ class "mb-10" ]
                                    |> Form.select nameAndIdFromString
                                        { parser =
                                            Form.Validate.succeed
                                                >> Form.Validate.required
                                                >> Form.Validate.validate translators
                                        , value = .city
                                        , update = \city input -> { input | city = city }
                                        , externalError = always Nothing
                                        }
             )
                |> Form.introspect
            )
        |> Form.with
            ((\{ city, state, country } ->
                let
                    availableDistricts =
                        Maybe.Extra.andThen2
                            (\validState validCity ->
                                List.Extra.find (\{ id } -> unwrapId id == validState.id)
                                    country.states
                                    |> Maybe.andThen
                                        (\{ cities } ->
                                            List.Extra.find
                                                (\{ id } -> unwrapId id == validCity.id)
                                                cities
                                        )
                                    |> Maybe.map .neighborhoods
                            )
                            state
                            city
                in
                case availableDistricts of
                    Nothing ->
                        Form.fail

                    Just districts ->
                        Form.Select.init
                            { label = t "register.form.district"
                            , id = "district-select"
                            , optionToString =
                                Maybe.map nameAndIdToString
                                    >> Maybe.withDefault ""
                            }
                            |> Form.Select.withOption Nothing (t "register.form.select.district")
                            |> Form.Select.withOptions
                                (List.map
                                    (\district ->
                                        { option = Just { id = unwrapId district.id, name = district.name }
                                        , label = district.name
                                        }
                                    )
                                    districts
                                )
                            |> Form.Select.withContainerAttrs [ class "mb-10" ]
                            |> Form.select nameAndIdFromString
                                { parser =
                                    Form.Validate.succeed
                                        >> Form.Validate.required
                                        >> Form.Validate.validate translators
                                , value = .district
                                , update = \district input -> { input | district = district }
                                , externalError = always Nothing
                                }
             )
                |> Form.introspect
            )
        |> Form.with
            (Form.Text.init
                { label = t "register.form.street.label"
                , id = "street-input"
                }
                |> Form.textField
                    { parser = Ok
                    , value = .street
                    , update = \street input -> { input | street = street }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init
                { label = t "register.form.number.label"
                , id = "address-number-input"
                }
                |> Form.textField
                    { parser = Ok
                    , value = .number
                    , update = \number input -> { input | number = number }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init
                { label = t "register.form.zip.label"
                , id = "zip-input"
                }
                |> Form.Text.withCounter (Form.Text.CountLetters 5)
                |> Form.textField
                    { parser =
                        \zip ->
                            if List.member zip costaRicaZipcodes then
                                Ok zip

                            else
                                Err <| t "error.zipcode"
                    , value = .zip
                    , update = \zip input -> { input | zip = zip }
                    , externalError = always Nothing
                    }
            )


nameAndIdToString : { id : String, name : String } -> String
nameAndIdToString { id, name } =
    id ++ ";" ++ name


nameAndIdFromString : String -> Maybe { id : String, name : String }
nameAndIdFromString nameAndId =
    case String.split ";" nameAndId of
        [ id, name ] ->
            Just { id = id, name = name }

        _ ->
            Nothing


init : Address.Country -> Form.Model FormInput
init country =
    Form.init
        { companyType = MIPYME
        , document = ""
        , name = ""
        , email = ""
        , phoneNumber = ""
        , state = Nothing
        , city = Nothing
        , district = Nothing
        , account = ""
        , street = ""
        , zip = ""
        , number = ""
        , country = country
        }


type CompanyType
    = MIPYME
    | Corporation


companyTypeToString : CompanyType -> String
companyTypeToString type_ =
    case type_ of
        MIPYME ->
            "mipyme"

        Corporation ->
            "gran_empresa"


companyTypeFromString : String -> Maybe CompanyType
companyTypeFromString type_ =
    case type_ of
        "mipyme" ->
            Just MIPYME

        "gran_empresa" ->
            Just Corporation

        _ ->
            Nothing


costaRicaZipcodes : List String
costaRicaZipcodes =
    [ "10101"
    , "10102"
    , "10103"
    , "10104"
    , "10105"
    , "10106"
    , "10107"
    , "10108"
    , "10109"
    , "10110"
    , "10111"
    , "10201"
    , "10202"
    , "10203"
    , "10301"
    , "10302"
    , "10303"
    , "10304"
    , "10305"
    , "10306"
    , "10307"
    , "10308"
    , "10309"
    , "10310"
    , "10311"
    , "10312"
    , "10313"
    , "10401"
    , "10402"
    , "10403"
    , "10404"
    , "10405"
    , "10406"
    , "10407"
    , "10408"
    , "10409"
    , "10501"
    , "10502"
    , "10503"
    , "10601"
    , "10602"
    , "10603"
    , "10604"
    , "10605"
    , "10606"
    , "10607"
    , "10701"
    , "10702"
    , "10703"
    , "10704"
    , "10705"
    , "10706"
    , "10707"
    , "10801"
    , "10802"
    , "10803"
    , "10804"
    , "10805"
    , "10806"
    , "10807"
    , "10901"
    , "10902"
    , "10903"
    , "10904"
    , "10905"
    , "10906"
    , "11001"
    , "11002"
    , "11003"
    , "11004"
    , "11005"
    , "11101"
    , "11102"
    , "11103"
    , "11104"
    , "11105"
    , "11201"
    , "11202"
    , "11203"
    , "11204"
    , "11205"
    , "11301"
    , "11302"
    , "11303"
    , "11304"
    , "11305"
    , "11401"
    , "11402"
    , "11403"
    , "11501"
    , "11502"
    , "11503"
    , "11504"
    , "11601"
    , "11602"
    , "11603"
    , "11604"
    , "11605"
    , "11701"
    , "11702"
    , "11703"
    , "11801"
    , "11802"
    , "11803"
    , "11804"
    , "11901"
    , "11902"
    , "11903"
    , "11904"
    , "11905"
    , "11906"
    , "11907"
    , "11908"
    , "11909"
    , "11910"
    , "11911"
    , "11912"
    , "12001"
    , "12002"
    , "12003"
    , "12004"
    , "12005"
    , "12006"
    , "20101"
    , "20102"
    , "20103"
    , "20104"
    , "20105"
    , "20106"
    , "20107"
    , "20108"
    , "20109"
    , "20110"
    , "20111"
    , "20112"
    , "20113"
    , "20114"
    , "20201"
    , "20202"
    , "20203"
    , "20204"
    , "20205"
    , "20206"
    , "20207"
    , "20208"
    , "20209"
    , "20210"
    , "20211"
    , "20212"
    , "20213"
    , "20214"
    , "20301"
    , "20302"
    , "20303"
    , "20304"
    , "20305"
    , "20307"
    , "20308"
    , "20401"
    , "20402"
    , "20403"
    , "20404"
    , "20501"
    , "20502"
    , "20503"
    , "20504"
    , "20505"
    , "20506"
    , "20507"
    , "20508"
    , "20601"
    , "20602"
    , "20603"
    , "20604"
    , "20605"
    , "20606"
    , "20607"
    , "20608"
    , "20701"
    , "20702"
    , "20703"
    , "20704"
    , "20705"
    , "20706"
    , "20707"
    , "20801"
    , "20802"
    , "20803"
    , "20804"
    , "20805"
    , "20901"
    , "20902"
    , "20903"
    , "20904"
    , "20905"
    , "21001"
    , "21002"
    , "21003"
    , "21004"
    , "21005"
    , "21006"
    , "21007"
    , "21008"
    , "21009"
    , "21010"
    , "21011"
    , "21012"
    , "21013"
    , "21101"
    , "21102"
    , "21103"
    , "21104"
    , "21105"
    , "21106"
    , "21107"
    , "21201"
    , "21202"
    , "21203"
    , "21204"
    , "21205"
    , "21301"
    , "21302"
    , "21303"
    , "21304"
    , "21305"
    , "21306"
    , "21307"
    , "21308"
    , "21401"
    , "21402"
    , "21403"
    , "21404"
    , "21501"
    , "21502"
    , "21503"
    , "21504"
    , "21601"
    , "21602"
    , "21603"
    , "30101"
    , "30102"
    , "30103"
    , "30104"
    , "30105"
    , "30106"
    , "30107"
    , "30108"
    , "30109"
    , "30110"
    , "30111"
    , "30201"
    , "30202"
    , "30203"
    , "30204"
    , "30205"
    , "30301"
    , "30302"
    , "30303"
    , "30304"
    , "30305"
    , "30306"
    , "30307"
    , "30308"
    , "30401"
    , "30402"
    , "30403"
    , "30501"
    , "30502"
    , "30503"
    , "30504"
    , "30505"
    , "30506"
    , "30507"
    , "30508"
    , "30509"
    , "30510"
    , "30511"
    , "30512"
    , "30601"
    , "30602"
    , "30603"
    , "30701"
    , "30702"
    , "30703"
    , "30704"
    , "30705"
    , "30801"
    , "30802"
    , "30803"
    , "30804"
    , "40101"
    , "40102"
    , "40103"
    , "40104"
    , "40105"
    , "40201"
    , "40202"
    , "40203"
    , "40204"
    , "40205"
    , "40206"
    , "40301"
    , "40302"
    , "40303"
    , "40304"
    , "40305"
    , "40306"
    , "40307"
    , "40308"
    , "40401"
    , "40402"
    , "40403"
    , "40404"
    , "40405"
    , "40406"
    , "40501"
    , "40502"
    , "40503"
    , "40504"
    , "40505"
    , "40601"
    , "40602"
    , "40603"
    , "40604"
    , "40701"
    , "40702"
    , "40703"
    , "40801"
    , "40802"
    , "40803"
    , "40901"
    , "40902"
    , "41001"
    , "41002"
    , "41003"
    , "41004"
    , "41005"
    , "50101"
    , "50102"
    , "50103"
    , "50104"
    , "50105"
    , "50201"
    , "50202"
    , "50203"
    , "50204"
    , "50205"
    , "50206"
    , "50207"
    , "50301"
    , "50302"
    , "50303"
    , "50304"
    , "50305"
    , "50306"
    , "50307"
    , "50308"
    , "50309"
    , "50401"
    , "50402"
    , "50403"
    , "50404"
    , "50501"
    , "50502"
    , "50503"
    , "50504"
    , "50601"
    , "50602"
    , "50603"
    , "50604"
    , "50605"
    , "50701"
    , "50702"
    , "50703"
    , "50704"
    , "50801"
    , "50802"
    , "50803"
    , "50804"
    , "50805"
    , "50806"
    , "50807"
    , "50808"
    , "50901"
    , "50902"
    , "50903"
    , "50904"
    , "50905"
    , "50906"
    , "51001"
    , "51002"
    , "51003"
    , "51004"
    , "51101"
    , "51102"
    , "51103"
    , "51104"
    , "51105"
    , "60101"
    , "60102"
    , "60103"
    , "60104"
    , "60105"
    , "60106"
    , "60107"
    , "60108"
    , "60109"
    , "60110"
    , "60111"
    , "60112"
    , "60113"
    , "60114"
    , "60115"
    , "60116"
    , "60201"
    , "60202"
    , "60203"
    , "60204"
    , "60205"
    , "60206"
    , "60301"
    , "60302"
    , "60303"
    , "60304"
    , "60305"
    , "60306"
    , "60307"
    , "60308"
    , "60309"
    , "60401"
    , "60402"
    , "60403"
    , "60501"
    , "60502"
    , "60503"
    , "60504"
    , "60505"
    , "60506"
    , "60601"
    , "60602"
    , "60603"
    , "60701"
    , "60702"
    , "60703"
    , "60704"
    , "60801"
    , "60802"
    , "60803"
    , "60804"
    , "60805"
    , "60806"
    , "60901"
    , "61001"
    , "61002"
    , "61003"
    , "61004"
    , "61101"
    , "61102"
    , "70101"
    , "70102"
    , "70103"
    , "70104"
    , "70201"
    , "70202"
    , "70203"
    , "70204"
    , "70205"
    , "70206"
    , "70207"
    , "70301"
    , "70302"
    , "70303"
    , "70304"
    , "70305"
    , "70306"
    , "70307"
    , "70401"
    , "70402"
    , "70403"
    , "70404"
    , "70501"
    , "70502"
    , "70503"
    , "70601"
    , "70602"
    , "70603"
    , "70604"
    , "70605"
    ]

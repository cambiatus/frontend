module Page.Register.JuridicalForm exposing (CompanyType(..), Field(..), Model, Msg(..), companyTypeToString, init, update, validator, view)

import Address
import Cambiatus.Scalar exposing (Id(..))
import Html exposing (Html)
import Html.Attributes exposing (autocomplete, classList)
import Kyc.CostaRica.Phone as KycPhone
import Page.Register.Common exposing (..)
import Session.Shared exposing (Translators)
import Validate exposing (Validator)
import View.Form.Input



--- MODEL


type alias Model =
    { companyType : CompanyType
    , document : String
    , name : String
    , email : String
    , phone : String
    , state : ( String, String )
    , city : ( String, String )
    , district : ( String, String )
    , account : String
    , street : String
    , zip : String
    , number : String
    , problems : List ( Field, String, ProblemEvent )
    , country : Address.Country
    , states : List Address.State
    , cities : List Address.City
    , districts : List Address.Neighborhood
    }


type Field
    = CompanyType
    | Document
    | Name
    | Email
    | Phone
    | State
    | City
    | District
    | Account
    | Street
    | Zip
    | Number


init : { a | account : Maybe String, email : Maybe String, phone : Maybe String, country : Address.Country } -> Translators -> Model
init options translators =
    { companyType = MIPYME
    , document = ""
    , name = ""
    , email = Maybe.withDefault "" options.email
    , phone = Maybe.withDefault "" options.phone
    , state = ( "", "" )
    , city = ( "", "" )
    , district = ( "", "" )
    , account = Maybe.withDefault "" options.account
    , street = ""
    , zip = ""
    , number = ""
    , problems = []
    , country = options.country
    , states = options.country.states ++ [ Address.State (Id "") (translators.t "register.form.select.state") [] ]
    , cities = []
    , districts = []
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



--- MSG


type Msg
    = EnteredDocument String
    | EnteredType CompanyType
    | EnteredName String
    | EnteredEmail String
    | EnteredPhone String
    | EnteredState String
    | EnteredCity String
    | EnteredDistrict String
    | EnteredAccount String
    | EnteredStreet String
    | EnteredZip String
    | EnteredNumber String



--- VIEW


view : Translators -> Model -> Html Msg
view translators model =
    let
        formTranslationString =
            "register.form"

        companyTranslationString =
            formTranslationString ++ ".company." ++ companyTypeToString model.companyType
    in
    Html.div []
        [ viewSelectField
            { id = "company_type_select"
            , label = translators.t "register.form.company_type"
            , onInput = EnteredType
            , options =
                [ { value = MIPYME, label = translators.t "register.form.company.mipyme.label" }
                , { value = Corporation, label = translators.t "register.form.company.gran_empresa.label" }
                ]
            , value = model.companyType
            , valueToString = companyTypeToString
            , enabled = True
            , problems = fieldProblems CompanyType model.problems
            }
        , View.Form.Input.init
            { id = "document"
            , label = translators.t (companyTranslationString ++ ".label")
            , onInput = EnteredDocument
            , disabled = False
            , value = model.document
            , placeholder = Just (translators.t (companyTranslationString ++ ".placeholder"))
            , problems = fieldProblems Document model.problems
            , translators = translators
            }
            |> View.Form.Input.withCounter
                (companyTranslationString
                    ++ ".maximum"
                    |> translators.t
                    |> String.toInt
                    |> Maybe.withDefault 10
                )
            |> View.Form.Input.toHtml
        , View.Form.Input.init
            { id = "name"
            , label = "Company Name"
            , disabled = False
            , onInput = EnteredName
            , placeholder = Just "Ex.: Cambiatus"
            , problems = fieldProblems Name model.problems
            , translators = translators
            , value = model.name
            }
            |> View.Form.Input.toHtml
        , View.Form.Input.init
            { id = "email"
            , label = translators.t (formTranslationString ++ ".email.label")
            , onInput = EnteredEmail
            , disabled = False
            , value = model.email
            , placeholder = Just (translators.t (formTranslationString ++ ".email.placeholder"))
            , problems = fieldProblems Email model.problems
            , translators = translators
            }
            |> View.Form.Input.toHtml
        , View.Form.Input.init
            { id = "phone"
            , label = translators.t (formTranslationString ++ ".phone.label")
            , onInput = EnteredPhone
            , disabled = False
            , value = model.phone
            , placeholder = Just (translators.t (formTranslationString ++ ".phone.placeholder"))
            , problems = fieldProblems Phone model.problems
            , translators = translators
            }
            |> View.Form.Input.withCounter 8
            |> View.Form.Input.toHtml
        , View.Form.Input.init
            { id = "account"
            , label = translators.t (formTranslationString ++ ".account.label")
            , onInput = EnteredAccount
            , disabled = False
            , value = model.account
            , placeholder = Just (translators.t (formTranslationString ++ ".account.placeholder"))
            , problems = fieldProblems Account model.problems
            , translators = translators
            }
            |> (let
                    hasErrors =
                        List.any (\( f, _, evt ) -> f == Account && evt == OnInput) model.problems
                in
                View.Form.Input.withAttrs
                    [ autocomplete False
                    , classList
                        [ ( "shake-invalid", hasErrors )
                        , ( "field-with-error", hasErrors )
                        ]
                    ]
               )
            |> View.Form.Input.withCounter 12
            |> View.Form.Input.toHtml
        , viewSelectField
            { id = "state_select"
            , label = translators.t "register.form.state"
            , onInput = EnteredState
            , options = List.map (\state -> { value = state.name, label = state.name }) model.states
            , value = model.state |> Tuple.first
            , valueToString = identity
            , enabled = True
            , problems = fieldProblems State model.problems
            }
        , viewSelectField
            { id = "city_select"
            , label = translators.t "register.form.city"
            , onInput = EnteredCity
            , options = List.map (\city -> { value = city.name, label = city.name }) model.cities
            , value = model.city |> Tuple.first
            , valueToString = identity
            , enabled = model.state /= ( "", "" )
            , problems = fieldProblems City model.problems
            }
        , viewSelectField
            { id = "discrict_select"
            , label = translators.t "register.form.district"
            , onInput = EnteredDistrict
            , options = List.map (\district -> { value = district.name, label = district.name }) model.districts
            , value = model.district |> Tuple.first
            , valueToString = identity
            , enabled = model.city /= ( "", "" )
            , problems = fieldProblems District model.problems
            }
        , View.Form.Input.init
            { id = "street"
            , label = translators.t (formTranslationString ++ ".street.label")
            , onInput = EnteredStreet
            , disabled = False
            , value = model.street
            , placeholder = Just (translators.t (formTranslationString ++ ".street.placeholder"))
            , problems = fieldProblems Street model.problems
            , translators = translators
            }
            |> View.Form.Input.toHtml
        , View.Form.Input.init
            { id = "number"
            , label = translators.t (formTranslationString ++ ".number.label")
            , onInput = EnteredNumber
            , disabled = False
            , value = model.number
            , placeholder = Just (translators.t (formTranslationString ++ ".number.placeholder"))
            , problems = fieldProblems Number model.problems
            , translators = translators
            }
            |> View.Form.Input.toHtml
        , View.Form.Input.init
            { id = "zip"
            , label = translators.t (formTranslationString ++ ".zip.label")
            , onInput = EnteredZip
            , disabled = False
            , value = model.zip
            , placeholder = Just (translators.t (formTranslationString ++ ".zip.placeholder"))
            , problems = fieldProblems Zip model.problems
            , translators = translators
            }
            |> View.Form.Input.withCounter 5
            |> View.Form.Input.toHtml
        ]



--- UPDATE


update : Translators -> Msg -> Model -> Model
update translators msg form =
    case msg of
        EnteredDocument document ->
            { form
                | document =
                    if String.length document <= 10 && not (containsLetters document) then
                        document

                    else
                        form.document
            }

        EnteredType type_ ->
            { form | companyType = type_ }

        EnteredName str ->
            { form | name = str }

        EnteredEmail str ->
            { form | email = str }

        EnteredPhone phone ->
            { form
                | phone =
                    if String.length phone <= 8 && not (containsLetters phone) then
                        phone

                    else
                        form.phone
            }

        EnteredState str ->
            { form
                | state = findId str form.country.states
                , cities = getCities form.country.states str translators
                , city = ( "", "" )
                , district = ( "", "" )
            }

        EnteredCity str ->
            { form
                | city = findId str form.cities
                , districts = getDistricts form.cities str translators
                , district = ( "", "" )
            }

        EnteredDistrict str ->
            { form | district = findId str form.districts }

        EnteredAccount account ->
            let
                formProblemsWithoutAccount =
                    form.problems
                        |> List.filter (\( field, _, _ ) -> field /= Account)

                ( preparedAccountName, errorMsg ) =
                    validateAccountName translators account form.account
            in
            { form
                | account = preparedAccountName
                , problems =
                    case errorMsg of
                        Nothing ->
                            formProblemsWithoutAccount

                        Just e ->
                            formProblemsWithoutAccount ++ [ ( Account, e, OnInput ) ]
            }

        EnteredStreet str ->
            { form | street = str }

        EnteredZip str ->
            { form
                | zip =
                    if String.length str <= 5 && not (containsLetters str) then
                        str

                    else
                        form.zip
            }

        EnteredNumber str ->
            { form | number = str }


validator : Translators -> Validator ( Field, String, ProblemEvent ) Model
validator { t, tr } =
    let
        ifInvalidPhoneNumber subjectToString error =
            Validate.ifFalse (\subject -> KycPhone.isValid (subjectToString subject)) error
    in
    Validate.all
        [ Validate.ifBlank .name ( Name, t "error.required", OnSubmit )
        , Validate.ifBlank .document ( Document, t "error.required", OnSubmit )
        , Validate.ifBlank .number ( Number, t "error.required", OnSubmit )
        , Validate.ifBlank .street ( Street, t "error.required", OnSubmit )
        , ifEmptyTuple .state ( State, t "error.required", OnSubmit )
        , ifEmptyTuple .city ( City, t "error.required", OnSubmit )
        , ifEmptyTuple .district ( District, t "error.required", OnSubmit )
        , Validate.firstError
            [ Validate.ifBlank .zip ( Zip, t "error.required", OnSubmit )
            , Validate.ifFalse (\f -> List.member f.zip costaRicaZipcodes) ( Zip, t "error.zipcode", OnSubmit )
            ]
        , Validate.firstError
            [ Validate.ifBlank .email ( Email, t "error.required", OnSubmit )
            , Validate.ifInvalidEmail .email (\_ -> ( Email, t "error.email", OnSubmit ))
            ]
        , Validate.firstError
            [ Validate.ifBlank .account ( Account, t "error.required", OnSubmit )
            , Validate.ifTrue (\f -> String.length f.account < 12) ( Account, tr "error.validator.text.exactly" [ ( "base", "12" ) ], OnSubmit )
            , Validate.ifTrue (\f -> String.length f.account > 12) ( Account, tr "error.validator.text.exactly" [ ( "base", "12" ) ], OnSubmit )
            , Validate.ifFalse (\f -> String.all Char.isAlphaNum f.account) ( Account, t "error.invalidChar", OnSubmit )
            ]
        , Validate.firstError
            [ Validate.ifBlank .phone ( Phone, t "error.required", OnSubmit )
            , ifInvalidPhoneNumber .phone ( Phone, t "error.phone", OnSubmit )
            ]
        ]


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

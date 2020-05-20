module Page.PaymentHistory exposing (Model, Msg, init, msgToString, update, view)

import Html exposing (Html, button, div, h1, h2, img, input, label, p, span, text, ul)
import Html.Attributes exposing (class, for, placeholder, src, style)
import Select
import Session.Guest as Guest exposing (External(..))
import Simple.Fuzzy
import UpdateResult as UR


init : Guest.Model -> ( Model, Cmd Msg )
init guest =
    ( { userSelectorState = Select.newState ""
      , selectedName = Nothing
      , users = []
      }
    , Cmd.none
    )


type alias ProfileTemp =
    { name : String
    }


type Msg
    = NoOp
    | OnSelect (Maybe ProfileTemp)
    | SelectMsg (Select.Msg ProfileTemp)


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        OnSelect _ ->
            [ "OnSelect" ]

        SelectMsg _ ->
            [ "SelectMsg" ]


type alias Model =
    { userSelectorState : Select.State
    , selectedName : Maybe String
    , users : List ProfileTemp
    }


selectConfig : Select.Config Msg ProfileTemp
selectConfig =
    Select.newConfig
        { onSelect = OnSelect
        , toLabel = .name
        , filter = filter 4 .name
        }


filter : Int -> (a -> String) -> String -> List a -> Maybe (List a)
filter minChars toLabel query items =
    if String.length query < minChars then
        Nothing

    else
        items
            |> Simple.Fuzzy.filter toLabel query
            |> Just


type alias UpdateResult =
    UR.UpdateResult Model Msg External


update : Msg -> Model -> Guest.Model -> UpdateResult
update msg model guest =
    case msg of
        OnSelect maybeUser ->
            let
                maybeName =
                    Maybe.map .name maybeUser
            in
            UR.init { model | selectedName = maybeName }

        SelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update selectConfig subMsg model.userSelectorState
            in
            UR.init { model | userSelectorState = updated }

        NoOp ->
            UR.init model


view : Guest.Model -> Model -> Html Msg
view guest model =
    div [ class "bg-white" ]
        [ viewSplash
        , div [ class "mx-4 max-w-md md:m-auto" ]
            [ h2 [ class "text-center text-black text-2xl" ] [ text "Payment History" ]
            , viewUserAutocomplete
            , viewPeriodSelector
            , viewPayersList
            , viewPagination
            ]
        ]


viewSplash =
    div
        [ class "bg-black bg-cover h-56 mb-6 flex justify-center items-center"
        , style "background-image" "url(/images/bg_cafe.png)"
        ]
        [ h1 [ class "text-white text-center text-5xl mx-3" ] [ text "Pura Vida Cafe" ]
        ]


viewUserAutocomplete =
    div [ class "my-4" ]
        [ label
            [ class "block" ]
            [ span [ class "text-green tracking-wide uppercase text-caption block mb-1" ]
                [ text "User" ]
            ]
        , input
            [ class "input min-w-full"
            , placeholder "Type username here"
            ]
            []
        ]


viewPeriodSelector =
    div [ class "my-4" ]
        [ label
            [ class "block" ]
            [ span [ class "text-green tracking-wide uppercase text-caption block mb-1" ]
                [ text "Period" ]
            ]
        , input
            [ class "input min-w-full"
            , placeholder "27 Oct 2020 (today)"
            ]
            []
        ]


viewPayment payment =
    div
        [ class "bg-gray-100 text-center py-6 my-6 rounded-lg"
        ]
        [ div [ class "rounded-full m-auto overflow-hidden border-white border-2 bg-grey w-14 h-14" ]
            [ img
                [ class "max-w-full max-h-full"
                , src payment.userpic
                ]
                []
            ]
        , p [ class "text-black mt-2" ]
            [ text payment.username ]
        , p [ class "uppercase text-gray-900 text-xs my-1" ]
            [ text payment.paymentDate ]
        , p [ class "text-green text-4xl my-3" ]
            [ text payment.paymentAmount ]
        , p [ class "tracking-widest text-2xl" ]
            [ text payment.emojiHash ]
        ]


viewPayersList =
    ul [ class "" ]
        (List.map viewPayment
            (List.repeat
                10
                { userpic = "/images/woman.png"
                , username = "vasya222"
                , paymentDate = "today, 14:03"
                , paymentAmount = "1234 COS"
                , emojiHash = "\u{1F916}🇨🇷💜😙😎💻😇🎃"
                }
            )
        )


viewPagination =
    div [ class "pb-8" ]
        [ button [ class "button m-auto button-primary w-full sm:w-40" ] [ text "Show More" ] ]

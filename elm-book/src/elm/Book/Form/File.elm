module Book.Form.File exposing (Model, Msg, chapter, initModel)

import Book.Helpers
import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import Form.File
import Form.File2
import Html exposing (Html)
import Html.Attributes
import Maybe.Extra
import UpdateResult



-- MODEL


type alias Model =
    { smallCircleExample : Form.File.Model
    , largeRectangleExample : Form.File.Model
    , largeRectangleGrayExample : Form.File.Model
    , file2 : Form.File2.Model
    }


initModel : Model
initModel =
    { smallCircleExample = Form.File.initModel Nothing
    , largeRectangleExample = Form.File.initModel Nothing
    , largeRectangleGrayExample = Form.File.initModel Nothing
    , file2 = Form.File2.initMultiple { images = [], aspectRatio = 1 / 1 }
    }



-- UPDATE


type Msg
    = GotSmallCircleMsg Form.File.Msg
    | GotLargeRectangleMsg Form.File.Msg
    | GotLargeRectangleGrayMsg Form.File.Msg
    | GotFile2Msg Form.File2.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSmallCircleMsg subMsg ->
            updateComponent subMsg
                model
                { fromModel = .smallCircleExample
                , toModel = \smallCircleExample model_ -> { model_ | smallCircleExample = smallCircleExample }
                , toMsg = GotSmallCircleMsg
                }

        GotLargeRectangleMsg subMsg ->
            updateComponent subMsg
                model
                { fromModel = .largeRectangleExample
                , toModel = \largeRectangleExample model_ -> { model_ | largeRectangleExample = largeRectangleExample }
                , toMsg = GotLargeRectangleMsg
                }

        GotLargeRectangleGrayMsg subMsg ->
            updateComponent subMsg
                model
                { fromModel = .largeRectangleGrayExample
                , toModel = \largeRectangleGrayExample model_ -> { model_ | largeRectangleGrayExample = largeRectangleGrayExample }
                , toMsg = GotLargeRectangleGrayMsg
                }

        GotFile2Msg subMsg ->
            updateComponent2 subMsg
                model
                { fromModel = .file2
                , toModel = \file2 model_ -> { model_ | file2 = file2 }
                , toMsg = GotFile2Msg
                }


updateComponent :
    Form.File.Msg
    -> Model
    ->
        { fromModel : Model -> Form.File.Model
        , toModel : Form.File.Model -> Model -> Model
        , toMsg : Form.File.Msg -> Msg
        }
    -> ( Model, Cmd Msg )
updateComponent msg model { fromModel, toModel, toMsg } =
    Form.File.update Book.Helpers.mockShared msg (fromModel model)
        |> UpdateResult.fromChild (\subModel -> toModel subModel model)
            toMsg
            UpdateResult.addExt
            model
        |> UpdateResult.toModelCmd
            (\_ m -> ( m, Cmd.none ))
            (\_ -> [])


updateComponent2 :
    Form.File2.Msg
    -> Model
    ->
        { fromModel : Model -> Form.File2.Model
        , toModel : Form.File2.Model -> Model -> Model
        , toMsg : Form.File2.Msg -> Msg
        }
    -> ( Model, Cmd Msg )
updateComponent2 msg model { fromModel, toModel, toMsg } =
    Form.File2.update Book.Helpers.mockShared msg (fromModel model)
        |> UpdateResult.fromChild (\subModel -> toModel subModel model)
            toMsg
            UpdateResult.addExt
            model
        |> UpdateResult.toModelCmd (\_ m -> ( m, Cmd.none ))
            (\_ -> [])



-- VIEW


viewSmallCircle : Model -> Html Msg
viewSmallCircle model =
    let
        options =
            Form.File.init
                { label = "Select a profile picture"
                , id = "small-circle-variant"
                }
                |> Form.File.withVariant Form.File.SmallCircle
    in
    Form.File.view options
        { value = model.smallCircleExample
        , error = Html.text ""
        , hasError = False
        , isRequired = True
        , translators = Book.Helpers.mockTranslators
        }
        GotSmallCircleMsg


viewLargeRectangle : Model -> Html Msg
viewLargeRectangle model =
    let
        options =
            Form.File.init
                { label = "Select a large file"
                , id = "large-rectangle-variant"
                }
    in
    Form.File.view options
        { value = model.largeRectangleExample
        , error = Html.text ""
        , hasError = False
        , isRequired = True
        , translators = Book.Helpers.mockTranslators
        }
        GotLargeRectangleMsg


viewFile2 : Model -> Html Msg
viewFile2 model =
    let
        options =
            Form.File2.init { id = "file2-example" }
                |> Form.File2.withMultipleFiles True
    in
    Form.File2.view options
        { value = model.file2
        , error = Html.text ""
        , hasError = False
        , isRequired = True
        , translators = Book.Helpers.mockTranslators
        }
        GotFile2Msg


viewLargeRectangleGray : Model -> Html Msg
viewLargeRectangleGray model =
    let
        options =
            Form.File.init
                { label = "Select a large file"
                , id = "large-rectangle-gray-variant"
                }
                |> Form.File.withVariant (Form.File.LargeRectangle Form.File.Gray)
    in
    Form.File.view options
        { value = model.largeRectangleGrayExample
        , error = Html.text ""
        , hasError = False
        , isRequired = True
        , translators = Book.Helpers.mockTranslators
        }
        GotLargeRectangleGrayMsg


viewSideBySide :
    Form.File.Options (ElmBook.Msg x)
    -> { image : String, error : Maybe String }
    -> Html (ElmBook.Msg x)
viewSideBySide options { image, error } =
    Html.div [ Html.Attributes.class "grid grid-cols-2 gap-4" ]
        [ Form.File.view options
            { value = Form.File.initModel Nothing
            , error = Book.Helpers.viewError [] (Maybe.Extra.isJust error) error
            , hasError = Maybe.Extra.isJust error
            , isRequired = True
            , translators = Book.Helpers.mockTranslators
            }
            (\_ -> Actions.logAction "Selected file")
        , Form.File.view options
            { value = Form.File.initModel (Just image)
            , error = Book.Helpers.viewError [] (Maybe.Extra.isJust error) error
            , hasError = Maybe.Extra.isJust error
            , isRequired = True
            , translators = Book.Helpers.mockTranslators
            }
            (\_ -> Actions.logAction "Selected file")
        ]


viewDisabledSmallCircle : Html (ElmBook.Msg x)
viewDisabledSmallCircle =
    let
        options =
            Form.File.init
                { label = "Select a profile picture"
                , id = "disabled-circle"
                }
                |> Form.File.withDisabled True
                |> Form.File.withVariant Form.File.SmallCircle
    in
    viewSideBySide options
        { image = "/images/satisfied-vagabonds.svg"
        , error = Nothing
        }


viewDisabledLargeRectangle : Html (ElmBook.Msg x)
viewDisabledLargeRectangle =
    let
        options =
            Form.File.init
                { label = "Select a large image"
                , id = "disabled-rectangle"
                }
                |> Form.File.withDisabled True
    in
    viewSideBySide options
        { image = "/images/auth_bg_full.png"
        , error = Nothing
        }


viewDisabledLargeGrayRectangle : Html (ElmBook.Msg x)
viewDisabledLargeGrayRectangle =
    let
        options =
            Form.File.init
                { label = "Select a large image"
                , id = "disabled-rectangle"
                }
                |> Form.File.withDisabled True
                |> Form.File.withVariant (Form.File.LargeRectangle Form.File.Gray)
    in
    viewSideBySide options
        { image = "/images/auth_bg_full.png"
        , error = Nothing
        }


viewSmallCircleWithError : Html (ElmBook.Msg x)
viewSmallCircleWithError =
    let
        options =
            Form.File.init
                { label = "Select a profile picture"
                , id = "error-circle"
                }
                |> Form.File.withVariant Form.File.SmallCircle
    in
    viewSideBySide options
        { image = "/images/satisfied-vagabonds.svg"
        , error = Just "Something wrong happened"
        }


viewLargeRectangleWithError : Html (ElmBook.Msg x)
viewLargeRectangleWithError =
    let
        options =
            Form.File.init
                { label = "Select a large image"
                , id = "error-rectangle"
                }
    in
    viewSideBySide options
        { image = "/images/auth_bg_full.png"
        , error = Just "Something wrong happened"
        }


viewLargeRectangleGrayWithError : Html (ElmBook.Msg x)
viewLargeRectangleGrayWithError =
    let
        options =
            Form.File.init
                { label = "Select a large image"
                , id = "error-rectangle"
                }
                |> Form.File.withVariant (Form.File.LargeRectangle Form.File.Gray)
    in
    viewSideBySide options
        { image = "/images/auth_bg_full.png"
        , error = Just "Something wrong happened"
        }



-- CHAPTER


mapToState : Html Msg -> Html (ElmBook.Msg { x | fileModel : Model })
mapToState =
    Html.map
        (Actions.mapUpdateWithCmd
            { fromState = .fileModel
            , toState = \state model -> { state | fileModel = model }
            , update = update
            }
        )


chapter : Chapter { x | fileModel : Model }
chapter =
    Chapter.chapter "File"
        |> Chapter.withComponentList
            [ ( "Disabled small circle"
              , viewDisabledSmallCircle
              )
            , ( "Disabled large rectangle"
              , viewDisabledLargeRectangle
              )
            , ( "Disabled gray large rectangle"
              , viewDisabledLargeGrayRectangle
              )
            , ( "Small circle with error"
              , viewSmallCircleWithError
              )
            , ( "Large rectangle with error"
              , viewLargeRectangleWithError
              )
            , ( "Gray large rectangle with error"
              , viewLargeRectangleGrayWithError
              )
            ]
        |> Chapter.withStatefulComponentList
            [ ( "Small circle variant"
              , \{ fileModel } ->
                    viewSmallCircle fileModel
                        |> mapToState
              )
            , ( "Large rectangle variant"
              , \{ fileModel } ->
                    viewLargeRectangle fileModel
                        |> mapToState
              )
            , ( "Large rectangle variant with gray background"
              , \{ fileModel } ->
                    viewLargeRectangleGray fileModel
                        |> mapToState
              )
            , ( "File2"
              , \{ fileModel } ->
                    viewFile2 fileModel
                        |> mapToState
              )
            ]
        |> Chapter.render """
Sometimes we need users to submit files, and this is where this component comes in!
It provides a nice interface to work with multiple kinds of image, such as PDFs
and images. Also, it automatically uploads the file to our servers, no wiring required!

It comes in two variants:

<component with-label="Small circle variant" />

The small circle variant is used mainly for uploading profile pictures, or the
logo of a community.

<component with-label="Large rectangle variant" />

<component with-label="Large rectangle variant with gray background" />

The large rectangle variant is usually used for PDF files, or larger pictures.

They can also be disabled:

<component with-label="Disabled small circle" />

<component with-label="Disabled large rectangle" />

<component with-label="Disabled gray large rectangle" />

And be in an error state:

<component with-label="Small circle with error" />

<component with-label="Large rectangle with error" />

<component with-label="Gray large rectangle with error" />

<component with-label="File2" />
"""

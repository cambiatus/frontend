module Book.Form.File exposing (Model, Msg, chapter, initModel)

import Book.Helpers
import ElmBook
import ElmBook.Actions as Actions
import ElmBook.Chapter as Chapter exposing (Chapter)
import Form.File
import Html exposing (Html)
import Html.Attributes exposing (class)
import Maybe.Extra
import UpdateResult



-- MODEL


type alias Model =
    { singleFileExample : Form.File.Model
    , multipleFilesExample : Form.File.Model
    , singleFileRoundedExample : Form.File.Model
    }


initModel : Model
initModel =
    { singleFileExample =
        Form.File.initSingle
            { fileUrl = Nothing
            , aspectRatio = Just 1
            }
            |> Form.File.fromSingleModel
    , multipleFilesExample =
        Form.File.initMultiple
            { fileUrls = []
            , aspectRatio = Just 1
            }
            |> Form.File.fromMultipleModel
    , singleFileRoundedExample =
        Form.File.initSingle
            { fileUrl = Nothing
            , aspectRatio = Just 1
            }
            |> Form.File.fromSingleModel
    }



-- UPDATE


type Msg
    = GotSingleFileMsg Form.File.Msg
    | GotMultipleFilesMsg Form.File.Msg
    | GotSingleFileRoundedMsg Form.File.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSingleFileMsg subMsg ->
            updateComponent subMsg
                model
                { fromModel = .singleFileExample
                , toModel = \singleFileExample model_ -> { model_ | singleFileExample = singleFileExample }
                , toMsg = GotSingleFileMsg
                }

        GotMultipleFilesMsg subMsg ->
            updateComponent subMsg
                model
                { fromModel = .multipleFilesExample
                , toModel = \multipleFilesExample model_ -> { model_ | multipleFilesExample = multipleFilesExample }
                , toMsg = GotMultipleFilesMsg
                }

        GotSingleFileRoundedMsg subMsg ->
            updateComponent subMsg
                model
                { fromModel = .singleFileRoundedExample
                , toModel = \singleFileRoundedExample model_ -> { model_ | singleFileRoundedExample = singleFileRoundedExample }
                , toMsg = GotSingleFileRoundedMsg
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



-- VIEW


viewSingleFile : Model -> Html Msg
viewSingleFile model =
    let
        options =
            Form.File.init { id = "single-file-example" }
    in
    Form.File.view options
        { value = model.singleFileExample
        , error = Html.text ""
        , hasError = False
        , isRequired = True
        , translators = Book.Helpers.mockTranslators
        }
        GotSingleFileMsg


viewMultipleFiles : Model -> Html Msg
viewMultipleFiles model =
    let
        options =
            Form.File.init { id = "multiple-files-example" }
    in
    Form.File.view options
        { value = model.multipleFilesExample
        , error = Html.text ""
        , hasError = False
        , isRequired = True
        , translators = Book.Helpers.mockTranslators
        }
        GotMultipleFilesMsg


viewSingleFileRounded : Model -> Html Msg
viewSingleFileRounded model =
    let
        options =
            Form.File.init { id = "single-file-rounded-example" }
                |> Form.File.withEntryContainerAttributes (\_ -> [ class "rounded-full focus-ring w-20 h-20" ])
                |> Form.File.withImageClass "rounded-full w-20 h-20"
                |> Form.File.withImageCropperAttributes [ class "rounded-full" ]
    in
    Form.File.view options
        { value = model.singleFileRoundedExample
        , error = Html.text ""
        , hasError = False
        , isRequired = True
        , translators = Book.Helpers.mockTranslators
        }
        GotSingleFileRoundedMsg


viewSideBySide :
    Form.File.Options (ElmBook.Msg x)
    -> { image : String, error : Maybe String }
    -> Html (ElmBook.Msg x)
viewSideBySide options { image, error } =
    Html.div [ class "grid grid-cols-2 gap-4" ]
        [ Form.File.view
            (options
                |> Form.File.withLabel "Without image selected"
            )
            { value =
                Form.File.initSingle { fileUrl = Nothing, aspectRatio = Nothing }
                    |> Form.File.fromSingleModel
            , error = Book.Helpers.viewError [] (Maybe.Extra.isJust error) error
            , hasError = Maybe.Extra.isJust error
            , isRequired = True
            , translators = Book.Helpers.mockTranslators
            }
            (\_ -> Actions.logAction "Selected file")
        , Form.File.view
            (options
                |> Form.File.withLabel "With image selected"
            )
            { value =
                Form.File.initSingle { fileUrl = Just image, aspectRatio = Nothing }
                    |> Form.File.fromSingleModel
            , error = Book.Helpers.viewError [] (Maybe.Extra.isJust error) error
            , hasError = Maybe.Extra.isJust error
            , isRequired = True
            , translators = Book.Helpers.mockTranslators
            }
            (\_ -> Actions.logAction "Selected file")
        ]


viewDisabled : Html (ElmBook.Msg x)
viewDisabled =
    let
        options =
            Form.File.init { id = "disabled-example" }
                |> Form.File.withDisabled True
    in
    viewSideBySide options
        { image = "/images/satisfied-vagabonds.svg"
        , error = Nothing
        }


viewWithError : Html (ElmBook.Msg x)
viewWithError =
    let
        options =
            Form.File.init { id = "error-example" }
    in
    viewSideBySide options
        { image = "/images/satisfied-vagabonds.svg"
        , error = Just "Something went wrong"
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
            [ ( "Disabled"
              , viewDisabled
              )
            , ( "With error"
              , viewWithError
              )
            ]
        |> Chapter.withStatefulComponentList
            [ ( "Single file"
              , \{ fileModel } ->
                    viewSingleFile fileModel
                        |> mapToState
              )
            , ( "Multiple files"
              , \{ fileModel } ->
                    viewMultipleFiles fileModel
                        |> mapToState
              )
            , ( "Single file rounded"
              , \{ fileModel } ->
                    viewSingleFileRounded fileModel
                        |> mapToState
              )
            ]
        |> Chapter.render """
Sometimes we need users to submit files, and this is where this component comes in!
It provides a nice interface to work with multiple kinds of files, such as PDFs
and images. Also, it automatically uploads the file to our servers, no wiring required!

For images, there is also the option to crop them (with a fixed aspect ratio).

We can choose between allowing multiple files to be uploaded at once, or just
one file at a time.

This component is highly customizable, as we might need very different looking
inputs in different places, so these examples aren't the only way these can look,
they're just what you get by default.

Here's an example for a single file:

<component with-label="Single file" />

And here's one for multiple files at once:

<component with-label="Multiple files" />

We can even show how it will look like with some rounded corners:

<component with-label="Single file rounded" />

They can also be disabled:

<component with-label="Disabled" />

And be in an error state:

<component with-label="With error" />
"""

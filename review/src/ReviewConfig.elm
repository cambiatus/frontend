module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

If you want to create a new rule, just run

    `elm-review new-rule`

inside the `review` directory.

-}

import NoBooleanCase
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoFunctionOutsideOfModules
import NoInconsistentEventLocation
import NoInconsistentJsAddressToMsg
import NoInconsistentMsgToString
import NoLeftPizza
import NoMissingTypeAnnotation
import NoModuleOnExposedNames
import NoRedundantConcat
import NoRedundantCons
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)
import Simplify
import UseCamelCase


config : List Rule
config =
    [ NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , NoUnused.Variables.rule
    , NoUnused.Modules.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoBooleanCase.rule
    , NoExposingEverything.rule
    , NoMissingTypeAnnotation.rule
    , Simplify.defaults
        |> Simplify.rule
    , NoLeftPizza.rule NoLeftPizza.Redundant
    , UseCamelCase.rule UseCamelCase.default
    , NoModuleOnExposedNames.rule
    , NoRedundantConcat.rule
    , NoInconsistentMsgToString.rule
    , NoInconsistentJsAddressToMsg.rule
    , NoInconsistentEventLocation.rule
    , NoRedundantCons.rule
    , NoFunctionOutsideOfModules.rule
        [ ( [ "Html.textarea" ], [ "Form.Text" ] )
        , ( [ "Html.input" ]
          , [ "Form.Text"
            , "Form.Checkbox"
            , "Form.Radio"
            , "Form.File"
            , "Form.Toggle"
            , "View.Select.Internal.Input"
            , "View.ImageCropper"
            ]
          )
        , ( [ "Html.label" ]
          , [ "Form.Checkbox"
            , "Form.Radio"
            , "Form.File"
            , "Form.Toggle"
            , "View.Components"
            ]
          )
        , ( [ "Html.select" ], [ "Form.Select" ] )
        , ( [ "Time.utc" ], [ "Session.Shared", "UtilsTests" ] )
        , ( [ "DatePicker.view", "DatePicker.initFromDate", "DatePicker.init" ]
          , [ "Form.DatePicker" ]
          )
        , ( [ "View.Select.newConfig", "View.Select.view", "View.Select.update" ]
          , [ "Form.UserPicker" ]
          )
        , ( [ "Slug.generate" ], [ "Utils" ] )
        ]
    ]
        -- Ignore generated code
        |> List.map (Rule.ignoreErrorsForDirectories [ "src/elm/Cambiatus", "src/elm/Select" ])

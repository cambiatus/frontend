module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import NoBooleanCase
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoFunctionOutsideOfModules
import NoInconsistentJsAddressToMsg
import NoInconsistentMsgToString
import NoLeftPizza
import NoMissingTypeAnnotation
import NoModuleOnExposedNames
import NoRedundantConcat
import NoRedundantCons
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
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
    , NoUnused.Exports.rule
        |> Rule.ignoreErrorsForFiles [ "src/elm/Select.elm", "src/elm/DataValidator.elm" ]
    , NoBooleanCase.rule
    , NoExposingEverything.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoMissingTypeAnnotation.rule
    , Simplify.defaults
        |> Simplify.ignoreCaseOfForTypes [ "View.Feedback.Msg", "I18Next.Delims", "Page.Profile.AddKyc.Msg" ]
        |> Simplify.rule
    , NoLeftPizza.rule NoLeftPizza.Redundant
    , UseCamelCase.rule UseCamelCase.default
    , NoModuleOnExposedNames.rule
    , NoRedundantConcat.rule
    , NoInconsistentMsgToString.rule
    , NoInconsistentJsAddressToMsg.rule
    , NoRedundantCons.rule
    , NoFunctionOutsideOfModules.rule "Html.input" [ "View.Form.Input", "View.Form.Toggle", "View.Form.Radio", "View.Form.FileUploader", "View.Form.Checkbox" ]
    , NoFunctionOutsideOfModules.rule "Html.select" [ "View.Form.Select" ]
    ]
        -- Ignore generated code
        |> List.map (Rule.ignoreErrorsForDirectories [ "src/elm/Cambiatus", "src/elm/Select" ])

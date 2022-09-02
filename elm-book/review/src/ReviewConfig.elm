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
    , Simplify.rule Simplify.defaults
    , NoLeftPizza.rule NoLeftPizza.Redundant
    , UseCamelCase.rule UseCamelCase.default
    , NoModuleOnExposedNames.rule
    , NoRedundantConcat.rule
    , NoRedundantCons.rule
    ]
        |> List.map (Rule.ignoreErrorsForDirectories [ "../src/elm" ])

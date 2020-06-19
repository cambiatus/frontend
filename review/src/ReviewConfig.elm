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
import NoUnused.Modules
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , NoUnused.Variables.rule
    , NoUnused.Modules.rule
    , NoBooleanCase.rule
    ]
        -- Ignore generated code
        |> List.map (Rule.ignoreErrorsForDirectories [ "src/elm/Cambiatus" ])

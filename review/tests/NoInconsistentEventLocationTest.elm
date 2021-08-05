module NoInconsistentEventLocationTest exposing (all)

import NoInconsistentEventLocation exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    let
        correctExplicit =
            """module A exposing (..)

import Log

func : Log.Event
func =
    { username = Nothing
    , message = "Some message"
    , tags = []
    , location = { moduleName = "A", function = "func" }
    , contexts = []
    , transaction = ""
    , level = Log.DebugLevel
    }
"""

        correctWithVariable =
            """module A exposing (..)

import Log

func : Log.Event
func =
    let
        location = { moduleName = "A", function = "func" }
    in
    { username = Nothing
    , message = "Some message"
    , tags = []
    , location = location
    , contexts = []
    , transaction = ""
    , level = Log.DebugLevel
    }
"""
    in
    describe "NoInconsistentEventLocation"
        [ test "should report an error when using an invalid moduleName in an explicit record" <|
            \() ->
                """module A exposing (..)

import Log

func : Log.Event
func =
    { username = Nothing
    , message = "Some message"
    , tags = []
    , location = { moduleName = "B", function = "func" }
    , contexts = []
    , transaction = ""
    , level = Log.DebugLevel
    }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The location should use the correct moduleName"
                            , details = [ "If we have an inconsistent location, it can be harder for us to debug later" ]
                            , under = "moduleName = \"B\""
                            }
                            |> Review.Test.whenFixed correctExplicit
                        ]
        , test "should report an error when using an invalid function name in an explicit record" <|
            \() ->
                """module A exposing (..)

import Log

func : Log.Event
func =
    { username = Nothing
    , message = "Some message"
    , tags = []
    , location = { moduleName = "A", function = "otherFunc" }
    , contexts = []
    , transaction = ""
    , level = Log.DebugLevel
    }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The location should use the correct function name"
                            , details = [ "If we have an inconsistent location, it can be harder for us to debug later" ]
                            , under = "function = \"otherFunc\""
                            }
                            |> Review.Test.whenFixed correctExplicit
                        ]
        , test "should report an error when using an invalid moduleName in a variable" <|
            \() ->
                """module A exposing (..)

import Log

func : Log.Event
func =
    let
        location = { moduleName = "B", function = "func" }
    in
    { username = Nothing
    , message = "Some message"
    , tags = []
    , location = location
    , contexts = []
    , transaction = ""
    , level = Log.DebugLevel
    }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The location should use the correct moduleName"
                            , details = [ "If we have an inconsistent location, it can be harder for us to debug later" ]
                            , under = "moduleName = \"B\""
                            }
                            |> Review.Test.whenFixed correctWithVariable
                        ]
        , test "should report an error when using an invalid function name in a variable" <|
            \() ->
                """module A exposing (..)

import Log

func : Log.Event
func =
    let
        location = { moduleName = "A", function = "otherFunc" }
    in
    { username = Nothing
    , message = "Some message"
    , tags = []
    , location = location
    , contexts = []
    , transaction = ""
    , level = Log.DebugLevel
    }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The location should use the correct function name"
                            , details = [ "If we have an inconsistent location, it can be harder for us to debug later" ]
                            , under = "function = \"otherFunc\""
                            }
                            |> Review.Test.whenFixed correctWithVariable
                        ]
        , test "should succeed when using a correct location with an explicit record" <|
            \() ->
                """module A exposing (..)

import Log

func : Log.Event
func =
    { username = Nothing
    , message = "Some message"
    , tags = []
    , location = { moduleName = "A", function = "func" }
    , contexts = []
    , transaction = ""
    , level = Log.DebugLevel
    }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should succeed when using a correct location with a variable" <|
            \() ->
                """module A exposing (..)

import Log

func : Log.Event
func =
    let
        location = { moduleName = "A", function = "func" }
    in
    { username = Nothing
    , message = "Some message"
    , tags = []
    , location = location
    , contexts = []
    , transaction = ""
    , level = Log.DebugLevel
    }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]

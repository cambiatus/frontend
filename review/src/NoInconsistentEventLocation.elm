module NoInconsistentEventLocation exposing (rule)

{-|

@docs rule

-}

import Dict
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Fix as Fix exposing (Fix)
import Review.Rule as Rule exposing (Rule)


{-| Reports when we use an inconsistent Location for `Log.Event`s

    config =
        [ NoInconsistentEventLocation.rule
        ]


## Fail

    module A exposing (..)

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


## Success

    module A exposing (..)

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

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoInconsistentEventLocation" contextCreator
        |> Rule.withDeclarationEnterVisitor declarationEnterVisitor
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.fromModuleRuleSchema



-- CONTEXT


contextCreator : Rule.ContextCreator () Context
contextCreator =
    Rule.initContextCreator
        (\metadata () ->
            { moduleName =
                Rule.moduleNameFromMetadata metadata
                    |> String.join "."
            , currentFunction = Nothing
            }
        )
        |> Rule.withMetadata


type alias Context =
    { moduleName : String
    , currentFunction : Maybe String
    }



-- VISITORS


declarationEnterVisitor : Node Declaration -> Context -> ( List (Rule.Error {}), Context )
declarationEnterVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            ( []
            , { context
                | currentFunction =
                    Node.value declaration
                        |> .name
                        |> Node.value
                        |> Just
              }
            )

        _ ->
            ( [], context )


expressionEnterVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionEnterVisitor node context =
    case Node.value node of
        Expression.RecordExpr setters ->
            case context.currentFunction of
                Nothing ->
                    ( [], context )

                Just currentFunction ->
                    let
                        asDict : Dict.Dict String { nameNode : Node String, expressionNode : Node Expression }
                        asDict =
                            setters
                                |> List.foldr
                                    (\currNode dict ->
                                        dict
                                            |> Dict.insert
                                                (Node.value currNode |> Tuple.first |> Node.value)
                                                { nameNode = Node.value currNode |> Tuple.first
                                                , expressionNode = Node.value currNode |> Tuple.second
                                                }
                                    )
                                    Dict.empty

                        isRightShape =
                            (asDict
                                |> Dict.keys
                                |> List.all (\key -> key == "moduleName" || key == "function")
                            )
                                && (asDict
                                        |> Dict.keys
                                        |> List.length
                                        |> (==) 2
                                   )
                    in
                    if isRightShape then
                        let
                            moduleError : Maybe (Rule.Error {})
                            moduleError =
                                Dict.get "moduleName" asDict
                                    |> Maybe.andThen
                                        (\{ nameNode, expressionNode } ->
                                            case Node.value expressionNode of
                                                Expression.Literal literalValue ->
                                                    if literalValue == context.moduleName then
                                                        Nothing

                                                    else
                                                        Rule.errorWithFix
                                                            { message = "The location should use the correct moduleName"
                                                            , details = [ "If we have an inconsistent location, it can be harder for us to debug later" ]
                                                            }
                                                            (Node.combine (\_ _ -> ()) expressionNode nameNode |> Node.range)
                                                            [ Fix.replaceRangeBy (Node.range expressionNode) ("\"" ++ context.moduleName ++ "\"") ]
                                                            |> Just

                                                _ ->
                                                    Nothing
                                        )

                            functionError : Maybe (Rule.Error {})
                            functionError =
                                Dict.get "function" asDict
                                    |> Maybe.andThen
                                        (\{ nameNode, expressionNode } ->
                                            case Node.value expressionNode of
                                                Expression.Literal literalValue ->
                                                    if literalValue == currentFunction then
                                                        Nothing

                                                    else
                                                        Rule.errorWithFix
                                                            { message = "The location should use the correct function name"
                                                            , details = [ "If we have an inconsistent location, it can be harder for us to debug later" ]
                                                            }
                                                            (Node.combine (\_ _ -> ()) expressionNode nameNode |> Node.range)
                                                            [ Fix.replaceRangeBy (Node.range expressionNode) ("\"" ++ currentFunction ++ "\"") ]
                                                            |> Just

                                                _ ->
                                                    Nothing
                                        )
                        in
                        ( [ moduleError, functionError ]
                            |> List.filterMap identity
                        , context
                        )

                    else
                        ( [], context )

        _ ->
            ( [], context )

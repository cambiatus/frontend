module Claim exposing (Model, selectionSet)

import Cambiatus.Enum.VerificationType exposing (VerificationType(..))
import Cambiatus.Object
import Cambiatus.Object.Action as Action
import Cambiatus.Object.Check as Check
import Cambiatus.Object.Claim as Claim
import Community exposing (Objective)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Profile exposing (Profile)


type alias Model =
    { id : Int
    , isVerified : Bool
    , claimer : Profile
    , action : Action
    , checks : List Check
    }


type alias Check =
    { isApproved : Bool
    , validator : Profile
    }


type alias Action =
    { id : Int
    , description : String
    , reward : Float
    , verifierReward : Float
    , validators : List Profile
    , verifications : Int
    , verificationType : VerificationType
    , objective : Objective
    }



-- GraphQL


selectionSet : SelectionSet Model Cambiatus.Object.Claim
selectionSet =
    SelectionSet.succeed Model
        |> with Claim.id
        |> with Claim.isVerified
        |> with (Claim.claimer Profile.selectionSet)
        |> with (Claim.action actionSelectionSet)
        |> with (Claim.checks (\_ -> { input = Absent }) checkSelectionSet)


actionSelectionSet : SelectionSet Action Cambiatus.Object.Action
actionSelectionSet =
    SelectionSet.succeed Action
        |> with Action.id
        |> with Action.description
        |> with Action.reward
        |> with Action.verifierReward
        |> with (Action.validators Profile.selectionSet)
        |> with Action.verifications
        |> with Action.verificationType
        |> with (Action.objective Community.objectiveSelectionSet)


checkSelectionSet : SelectionSet Check Cambiatus.Object.Check
checkSelectionSet =
    SelectionSet.succeed Check
        |> with Check.isVerified
        |> with (Check.validator Profile.selectionSet)

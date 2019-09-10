module Api.Dashboard.Activities exposing (ActivitiesResponse, Activity, activitiesSelectionSet, mapToActivity)

import Bespiral.Enum.VerificationType
import Bespiral.Object
import Bespiral.Object.Action as Action
import Bespiral.Object.Check as Check
import Bespiral.Object.Claim as Claim exposing (ChecksOptionalArguments)
import Bespiral.Object.Community as Community
import Bespiral.Object.Objective as Objective exposing (ActionsOptionalArguments)
import Bespiral.Object.Profile as Profile
import Bespiral.Query
import Bespiral.Scalar exposing (DateTime)
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import List.Extra
import View.Tag exposing (TagStatus(..))


type alias Activity =
    { communityLogo : String
    , communityName : String
    , objectiveDescription : String
    , actionDescription : String
    , verifierReward : Float
    , claimCreatedAt : DateTime
    , claimerAccount : String
    , claimerAvatar : Maybe String
    , claimerReward : Float
    , status : TagStatus
    }


type alias ActivitiesResponse =
    { communities : List Community
    }


type alias Community =
    { logo : String
    , name : String
    , objectives : List Objective
    }


type alias Objective =
    { description : String
    , actions : List Action
    }


type alias Action =
    { description : String
    , reward : Float
    , verifierReward : Float
    , claims : List Claim
    }


type alias Claim =
    { id : Int
    , claimer : Claimer
    , checks : List Check
    , createdAt : DateTime
    }


type alias Claimer =
    { account : String
    , avatar : Maybe String
    }


type alias Check =
    { isVerified : Bool
    }


type alias Account =
    String


mapToActivity : ActivitiesResponse -> List Activity
mapToActivity activitiesResponse =
    let
        toActivity : Community -> Objective -> Action -> Claim -> Activity
        toActivity community objective action claim =
            { communityLogo = community.logo
            , communityName = community.name
            , objectiveDescription = objective.description
            , actionDescription = action.description
            , verifierReward = action.verifierReward
            , claimCreatedAt = claim.createdAt
            , claimerAccount = claim.claimer.account
            , claimerAvatar = claim.claimer.avatar
            , claimerReward = action.reward
            , status =
                case List.head claim.checks of
                    Just check ->
                        if check.isVerified then
                            APPROVED

                        else
                            DISAPPROVED

                    Nothing ->
                        PENDING
            }

        toClaim : Community -> Objective -> Action -> List Activity
        toClaim community objective action =
            List.map
                (toActivity community objective action)
                action.claims

        toAction : Community -> Objective -> List Activity
        toAction community objective =
            List.Extra.andThen
                (toClaim community objective)
                objective.actions

        toObjective : Community -> List Activity
        toObjective community =
            List.Extra.andThen
                (toAction community)
                community.objectives
    in
    List.Extra.andThen
        toObjective
        activitiesResponse.communities


activitiesSelectionSet : Account -> SelectionSet ActivitiesResponse RootQuery
activitiesSelectionSet account =
    let
        selectionSet =
            communitySelectionSet account
    in
    SelectionSet.succeed ActivitiesResponse
        |> with (Bespiral.Query.communities selectionSet)


communitySelectionSet : Account -> SelectionSet Community Bespiral.Object.Community
communitySelectionSet account =
    let
        selectionSet =
            objectiveSelectionSet account
    in
    SelectionSet.succeed Community
        |> with Community.logo
        |> with Community.name
        |> with (Community.objectives selectionSet)


objectiveSelectionSet : Account -> SelectionSet Objective Bespiral.Object.Objective
objectiveSelectionSet account =
    let
        optionalArgs : ActionsOptionalArguments -> ActionsOptionalArguments
        optionalArgs _ =
            { input =
                Present
                    { creator = Absent
                    , isCompleted = Absent
                    , validator = Present account
                    , verificationType = Present Bespiral.Enum.VerificationType.Claimable
                    }
            }

        selectionSet : SelectionSet Action Bespiral.Object.Action
        selectionSet =
            actionSelectionSet account
    in
    SelectionSet.succeed Objective
        |> with Objective.description
        |> with (Objective.actions optionalArgs selectionSet)


actionSelectionSet : Account -> SelectionSet Action Bespiral.Object.Action
actionSelectionSet account =
    let
        selectionSet : SelectionSet Claim Bespiral.Object.Claim
        selectionSet =
            claimSelectionSet account
    in
    SelectionSet.succeed Action
        |> with Action.description
        |> with Action.reward
        |> with Action.verifierReward
        |> with (Action.claims selectionSet)


claimSelectionSet : Account -> SelectionSet Claim Bespiral.Object.Claim
claimSelectionSet account =
    let
        optionalArgs : ChecksOptionalArguments -> ChecksOptionalArguments
        optionalArgs _ =
            { input =
                Present { validator = Present account }
            }
    in
    SelectionSet.succeed Claim
        |> with Claim.id
        |> with (Claim.claimer claimerSelectionSet)
        |> with (Claim.checks optionalArgs checksSelectionSet)
        |> with Claim.createdAt


claimerSelectionSet : SelectionSet Claimer Bespiral.Object.Profile
claimerSelectionSet =
    SelectionSet.succeed Claimer
        |> with Profile.account
        |> with Profile.avatar


checksSelectionSet : SelectionSet Check Bespiral.Object.Check
checksSelectionSet =
    SelectionSet.succeed Check
        |> with Check.isVerified

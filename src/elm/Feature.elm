module Feature exposing (Feature(..), FeatureFlags(..), add, has, remove)


type FeatureFlags
    = FeatureFlags (List Feature)


type Feature
    = Shop
    | Actions


add : Feature -> FeatureFlags -> FeatureFlags
add feature (FeatureFlags flags) =
    if List.member feature flags then
        FeatureFlags flags

    else
        FeatureFlags (feature :: flags)


remove : Feature -> FeatureFlags -> FeatureFlags
remove feature (FeatureFlags flags) =
    FeatureFlags (List.filter ((/=) feature) flags)


has : Feature -> FeatureFlags -> Bool
has feature (FeatureFlags flags) =
    List.member feature flags

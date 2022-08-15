module Constants exposing (defaultCommunityLogo)

{-| Here's where we store some constants that we can use in the app.

It's usually better to have them in the module they make more sense in. For
example, `defaultCommunityLogo` would be better in `Community.elm`, but if we
put it there, we'd have some import cycles. We can use this module to avoid that.

-}


defaultCommunityLogo : String
defaultCommunityLogo =
    "https://cambiatus-uploads.s3.amazonaws.com/cambiatus-uploads/community_2.png"

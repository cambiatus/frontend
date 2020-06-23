module Emoji exposing (encode)

import Dict exposing (Dict)
import Hashids
import Murmur3


{-| Convert Transfer identifier (64 symbols) to emoji sequence (8 symbols)
-}
encode : String -> String
encode transferId =
    transferId
        |> Murmur3.hashString salt
        -- make 32 bit number
        |> Hashids.encode hashidsCtx
        -- make short has from the given alphabet
        |> String.split ""
        |> List.map (\n -> Maybe.withDefault "" (Dict.get n alphabetEmojiMapper))
        -- map hash symbols to emojis
        |> String.join " "


salt : Int
salt =
    123456


alphabet : String
alphabet =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"


hashidsCtx : Hashids.Context
hashidsCtx =
    Hashids.createHashidsContext (String.fromInt salt) 8 alphabet


alphabetEmojiMapper : Dict String String
alphabetEmojiMapper =
    let
        emojis =
            -- Must be separated by the space
            "😄 😘 😺 🚀 🚗 🚙 🚚 🌀 🌟 🌴 🌵 🌷 🌸 🌹 🌺 🌻 🌼 🌽 🌿 🍁 🍄 🍅 🍆 🍇 🍈 🍉 🍊 🍌 🍍 🍎 🍏 🍑 🍒 🍓 🍭 🍯 🎀 🎃 🎈 🎨 🎲 🎸 🏡 🐌 🐒 🐚 🐞 🐬 🐱 🐲 🐳 🐴 🐵 🐶 🐸 🐹 💜 😎 🚘 🌳 🍋 🍐"
    in
    List.map2 Tuple.pair (String.split "" alphabet) (String.split " " emojis)
        |> Dict.fromList

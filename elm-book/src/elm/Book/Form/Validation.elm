module Book.Form.Validation exposing (chapter)

import ElmBook.Chapter as Chapter exposing (Chapter)



-- CHAPTER


chapter : Chapter x
chapter =
    Chapter.chapter "Validation"
        |> Chapter.withComponentList []
        |> Chapter.withStatefulComponentList []
        |> Chapter.render """
As discussed in the [Form Introduction chapter](/forms/introduction), we want to
follow [Parse don't validate](https://sporto.github.io/elm-patterns/basic/parse-dont-validate.html),
and for that, we have a nice validation API that goes along very well with our forms.

Most (if not all) form components need a `parse` function, which takes in the
value the component usually takes in (i.e. a [Text input](/form/text) will take in a `String`),
and returns a parsed version of that value, which could be any type. Of course,
validation can fail, so the `parse` function actually produces a `Result String parsedType`,
where the `String` is a (translated) error message. So, all that to say that we
need to give form components a function that looks like this:

```elm
parse : rawType -> Result String parsedType
```

Which is nothing magic! You can use any regular function that will fit that signature.
However, the `Form.Validate` module offers a more standard way of doing things,
standard error messages (but they *can* be customized if you want), and a bunch of
helper functions which are very common to be needed. It operates on an opaque type,
`Validator`, which represents the result of parsing something, and it looks
exactly like we might expect:

```elm
type Validator output =
    Validator (Result Error output)
```

The only difference being that `Error` is actually a function that receives `Translators`
as input, and returns a translated `String`. The reason we don't just have a `String`
instead of `Error` is that the code that calls the validator only needs to provide
`Translators` once, and we can use those `Translators` only when finally parsing
the data.

---

The `Validator` module also uses a pipeline approach to its API:

```elm
parse =
    -- We start the `Validator` with `succeed`
    Form.Validate.succeed
        -- We apply some validations that change the input type
        -- `int` turns a `String` input into an `Int`
        >> Form.Validate.int
        -- Now that we have an `Int`, we can check if it's greater than 0
        >> Form.Validate.intGreaterThan 0
        -- We can implement our own parsing function!
        >> Form.Validate.custom
            (\\intParameter ->
                if intParameter > 100 then
                    -- If there is a parsing error, we can translate it
                    -- We could also use `Form.Validate.intLowerThan`
                    Err (\\translators_ -> translators_.t "error.number_too_high")

                else if modBy 3 intParameter && modBy 5 intParameter then
                    Ok "FizzBuzz"

                else if modBy 3 intParameter then
                    Ok "Fizz"

                else if modBy 5 intParameter then
                    Ok "Buzz"

                else
                    String.fromInt intParameter
            )
        -- Now that we have applied everything we needed, we can validate the data
        -- Note that we need to give it some `Translators`
        >> Form.Validate.validate translators
```

Note that we used the `>>` pipe instead of `|>`. We could have used `|>` if we wanted:

```elm
parse value =
    Form.Validate.succeed value
        |> ...
```
"""

module Page.Register.Common exposing
    ( accountNameField
    , emailField
    , personNameField
    , phoneNumberField
    )

import Eos.Account
import Form
import Form.Text
import Form.Validate
import Kyc.CostaRica.Phone
import Session.Shared exposing (Translators)
import Set exposing (Set)


emailField : Translators -> Form.Form msg { formInput | email : String } String
emailField ({ t } as translators) =
    Form.Text.init { label = t "register.form.email.label", id = "email-input" }
        |> Form.Text.withPlaceholder (t "register.form.email.placeholder")
        |> Form.textField
            { parser =
                Form.Validate.succeed
                    >> Form.Validate.email
                    >> Form.Validate.validate translators
            , value = .email
            , update = \email input -> { input | email = email }
            , externalError = always Nothing
            }


personNameField : Translators -> Form.Form msg { formInput | name : String } String
personNameField { t } =
    Form.Text.init
        { label = t "register.form.name.label"
        , id = "name-input"
        }
        |> Form.Text.withPlaceholder (t "register.form.name.placeholder")
        |> Form.textField
            { parser = Ok
            , value = .name
            , update = \name input -> { input | name = name }
            , externalError = always Nothing
            }


accountNameField : Translators -> { unavailableAccounts : Set String } -> Form.Form msg { formInput | account : String } Eos.Account.Name
accountNameField ({ t } as translators) { unavailableAccounts } =
    Form.Text.init
        { label = t "register.form.account.label"
        , id = "account-input"
        }
        |> Form.Text.withPlaceholder (t "register.form.account.placeholder")
        |> Form.Text.withCounter (Form.Text.CountLetters 12)
        |> Form.textField
            { parser =
                Form.Validate.succeed
                    >> Form.Validate.eosName
                    >> Form.Validate.custom
                        (\accountName ->
                            if Set.member (Eos.Account.nameToString accountName) unavailableAccounts then
                                Err (\translators_ -> translators_.t "error.alreadyTaken")

                            else
                                Ok accountName
                        )
                    >> Form.Validate.validate translators
            , value = .account
            , update = \account input -> { input | account = account }
            , externalError = always Nothing
            }


phoneNumberField : Translators -> Form.Form msg { formInput | phoneNumber : String } String
phoneNumberField { t } =
    Form.Text.init
        { label = t "register.form.phone.label"
        , id = "phone-input"
        }
        |> Form.Text.withPlaceholder (t "register.form.phone.placeholder")
        |> Form.Text.withCounter (Form.Text.CountLetters 8)
        |> Form.Text.withType Form.Text.Telephone
        |> Form.textField
            { parser =
                \phoneNumber ->
                    if Kyc.CostaRica.Phone.isValid phoneNumber then
                        Ok phoneNumber

                    else
                        Err <| t "error.phone"
            , value = .phoneNumber
            , update = \phoneNumber input -> { input | phoneNumber = phoneNumber }
            , externalError = always Nothing
            }

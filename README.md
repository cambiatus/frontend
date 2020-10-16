# Cambiatus frontend

This project is bootstrapped with [Create Elm App](https://github.com/halfzebra/create-elm-app).


You must have yarn installed to develop or build.


## Development

```
yarn install
yarn start
```

To update the GraphQL Elm files, run:

```
yarn generate-graphql
```

## Code Quality

We check JS with [StandardJS](https://github.com/standard/standard) and Elm with [elm-review](https://github.com/jfmengels/elm-review)
before the `git push` using [husky](https://github.com/typicode/husky). You can also run `yarn standard` and `yarn review`
at any moment to make sure your code is passing the linters.

You can also use [`elm-analyse`](https://github.com/stil4m/elm-analyse) to get even more insights about the code.
This package is not included in the repo, you should [install it manually](https://github.com/stil4m/elm-analyse#install):

```sh
yarn global add elm-analyse

elm-analyse              # view analyse result in terminal
elm-analyse -p 3001 -s   # show extended analyse in the browser on `localhost:3001`
```


## Build

```
yarn install
yarn build
```

## Configurations

Our app have a somewhat complex configuration stack, allowing the app to run without any changes, connecting to the staging environmnent. Here is an outline of all the configuration files and how the interact:

- `src/scripts/config.js` is the bottom level config. All the defaults are stored here for `DEV` and `PROD` environments.
- `env-config.js` is the config for the dev/prod/demo servers. This file overwrites the defaults from the `config.js` above. Currently, this file _is not used_ in the local dev environment, on the localhost it won't be loaded correctly. In the repo, this file contains the data for the Staging server (in Demo it will be different, in a dedicated server for a community it will be different, etc.).
- `src/elm/Flags.elm` is the Elm configuration file. We start our Elm app and send a port with all that comes from `src/scripts/config.js`. Since we need to decode JSON values on Elm, it comes with some default values so the app won't fail to load, we default it to some values specified there.


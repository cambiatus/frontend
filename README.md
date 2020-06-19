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


## Build

```
yarn install
yarn build
```

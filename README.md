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

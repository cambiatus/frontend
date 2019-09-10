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

Add the linter pre-commit hook:

```
vim .git/hooks/pre-commit
```

And add the following:

```shell
#!/bin/bash

# Ensure all JavaScript files staged for commit pass standard code style
function xargs-r() {
  # Portable version of "xargs -r". The -r flag is a GNU extension that
  # prevents xargs from running if there are no input files.
  if IFS= read -r -d $'\n' path; then
    { echo "$path"; cat; } | xargs $@
  fi
}
git diff --name-only --cached --relative | grep '\.jsx\?$' | sed 's/[^[:alnum:]]/\\&/g' | xargs-r -E '' -t node_modules/.bin/standard
if [[ $? -ne 0 ]]; then
  echo 'JavaScript Standard Style errors were detected. Aborting commit.'
  exit 1
fi
```

## Build


```
yarn install
yarn build
```

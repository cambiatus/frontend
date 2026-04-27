module.exports = {
  root: true,
  parser: '@babel/eslint-parser',
  parserOptions: {
    requireConfigFile: false,
    babelOptions: {
      babelrc: true
    }
  },
  extends: 'standard',
  plugins: ['html']
}

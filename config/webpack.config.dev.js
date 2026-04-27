"use strict";

// const autoprefixer = require('autoprefixer')
const path = require("path");
const webpack = require("webpack");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const CaseSensitivePathsPlugin = require("case-sensitive-paths-webpack-plugin");
const InterpolateHtmlPlugin = require("react-dev-utils/InterpolateHtmlPlugin");
const GitHashWebpackPlugin = require("git-hash-webpack-plugin");
const getClientEnvironment = require("./env");
const paths = require("../config/paths");

// Webpack uses `publicPath` to determine where the app is being served from.
// In development, we always serve from the root. This makes config easier.
const publicPath = "/";
// `publicUrl` is just like `publicPath`, but we will provide it to our app
// as %PUBLIC_URL% in `index.html` and `process.env.PUBLIC_URL` in JavaScript.
// Omit trailing slash as %PUBLIC_URL%/xyz looks better than %PUBLIC_URL%xyz.
const publicUrl = "";
// Get environment variables to inject into our app.
const env = getClientEnvironment(publicUrl);

// This is the development configuration.
// It is focused on developer experience and fast rebuilds.
// The production configuration is different and lives in a separate file.
module.exports = {
  mode: "development",
  // You may want 'eval' instead if you prefer to see the compiled output in DevTools.
  // See the discussion in https://github.com/facebook/create-react-app/issues/343.
  devtool: "cheap-module-source-map",
  // Don't fail on warnings - eosjs has known ajv schema issues that don't affect runtime
  bail: false,
  // These are the "entry points" to our application.
  // This means they will be the "root" imports that are included in JS bundle.
  // The first two entry points enable "hot" CSS and auto-refreshes for JS.
  entry: [
    // We ship a few polyfills by default:
    require.resolve("./polyfills"),
    // Include an alternative client for WebpackDevServer. A client's job is to
    // connect to WebpackDevServer by a socket and get notified about changes.
    // When you save a file, the client will either apply hot updates (in case
    // of CSS changes), or refresh the page (in case of JS changes). When you
    // make a syntax error, this client will display a syntax error overlay.
    // Note: instead of the default WebpackDevServer client, we use a custom one
    // to bring better experience for Create Elm App users. You can replace
    // the line below with these two lines if you prefer the stock client:
    // require.resolve('webpack-dev-server/client') + '?/',
    // require.resolve('webpack/hot/dev-server'),
    // require.resolve('react-dev-utils/webpackHotDevClient'),
    require.resolve("../scripts/utils/webpackHotDevClient"),

    // Errors should be considered fatal in development
    require.resolve("react-error-overlay"),

    paths.appIndexJs,
  ],
  output: {
    // Add /* filename */ comments to generated require()s in the output.
    pathinfo: true,
    // This does not produce a real file. It's just the virtual path that is
    // served by WebpackDevServer in development. This is the JS bundle
    // containing code from all our entry points, and the Webpack runtime.
    filename: "static/js/[name].js",
    // There are also additional JS chunk files if you use code splitting.
    chunkFilename: "static/js/[name].chunk.js",
    // This is the URL that app is served from. We use "/" in development.
    publicPath: publicPath,
    // Point sourcemap entries to original disk location (format as URL on Windows)
    devtoolModuleFilenameTemplate: (info) =>
      path.resolve(info.absoluteResourcePath).replace(/\\/g, "/"),
  },
  optimization: {
    // Automatically split vendor and commons
    // https://twitter.com/wSokra/status/969633336732905474
    // https://medium.com/webpack/webpack-4-code-splitting-chunk-graph-and-the-splitchunks-optimization-be739a861366
    splitChunks: {
      chunks: "all",
      name: "vendors",
    },
    // Keep the runtime chunk seperated to enable long term caching
    // https://twitter.com/wSokra/status/969679223278505985
    runtimeChunk: true,
  },
  resolve: {
    modules: ["node_modules"],
    extensions: [".js", ".elm", ".mjs"],
    alias: {
      // Point ajv schema references to the actual webpack schema
      // This is a workaround for https://github.com/EOSIO/eosjs/issues/1063
      WebpackOptions: path.resolve(
        __dirname,
        "../node_modules/webpack/schemas/WebpackOptions.json",
      ),
    },
    fallback: {
      stream: require.resolve("stream-browserify"),
      crypto: require.resolve("crypto-browserify"),
      buffer: require.resolve("buffer"),
      util: require.resolve("util"),
      process: require.resolve("process/browser"),
      vm: false,
    },
  },
  module: {
    noParse: /\.elm$/,
    // Disabled strictExportPresence to allow eosjs/ajv to work despite schema resolution issues
    strictExportPresence: false,
    rules: [
      // Disable require.ensure as it's not a standard language feature.
      { parser: { requireEnsure: false } },
      // Allow .mjs files to resolve without fully-specified paths
      {
        test: /\.m?js$/,
        resolve: {
          fullySpecified: false,
        },
      },
      {
        test: /\.js$/,
        exclude: [/[/\\\\]elm-stuff[/\\\\]/, /[/\\\\]node_modules[/\\\\]/],
        include: paths.appSrc,
        loader: require.resolve("babel-loader"),
        options: {
          presets: [
            [
              require.resolve("@babel/preset-env"),
              {
                // Let babel-preset-env use browserslist automatically
                // `usage` performs static code analysis to determine what's required
                useBuiltIns: "usage",
                corejs: 3,
                // Do not transform modules to CJS
                modules: false,
              },
            ],
          ],
          plugins: [
            // Polyfills the runtime needed for async/await and generators
            [
              require("@babel/plugin-transform-runtime").default,
              {
                helpers: true,
                regenerator: false,
              },
            ],
          ],
        },
      },
      // Process any JS outside of the app with Babel.
      // Unlike the application JS, we only compile the standard ES features.
      {
        test: /\.js$/,
        use: [
          {
            loader: require.resolve("babel-loader"),
            options: {
              babelrc: false,
              compact: false,
              presets: [
                [
                  // Latest stable ECMAScript features
                  require("@babel/preset-env").default,
                  {
                    // Do not transform modules to CJS
                    modules: false,
                  },
                ],
              ],
              cacheDirectory: true,
              highlightCode: true,
            },
          },
        ],
      },
      {
        test: /\.elm$/,
        include: paths.appSrc,
        exclude: [/[/\\\\]elm-stuff[/\\\\]/, /[/\\\\]node_modules[/\\\\]/],
        use: [
          {
            loader: require.resolve("elm-hot-webpack-loader"),
          },
          // string-replace-loader works as InterpolateHtmlPlugin for Elm,
          // it replaces all of the %PUBLIC_URL% with the URL of your
          // application, so you could serve static assets outside of the
          // module system.
          {
            loader: require.resolve("string-replace-loader"),
            options: {
              search: "%PUBLIC_URL%",
              replace: publicUrl,
              flags: "g",
            },
          },
          {
            loader: require.resolve("elm-webpack-loader"),
            options: {
              verbose: true,
              // If ELM_DEBUGGER was set to "false", disable it. Otherwise
              // for invalid values, "true" and as a default, enable it
              debug: process.env.ELM_DEBUGGER !== "false",
              pathToElm: paths.elm,
            },
          },
        ],
      },

      // "postcss" loader applies autoprefixer to our CSS.
      // "css" loader resolves paths in CSS and adds assets as dependencies.
      // "style" loader turns CSS into JS modules that inject <style> tags.
      // In production, we use a plugin to extract that CSS to a file, but
      // in development "style" loader enables hot editing of CSS.
      // By default we support CSS Modules with the extension .module.css
      {
        test: /\.css/,
        use: [
          require.resolve("style-loader"),
          {
            loader: require.resolve("css-loader"),
            options: {
              importLoaders: 1,
              sourceMap: true,
              url: {
                filter: (url) => !url.startsWith("/"),
              },
            },
          },
          {
            loader: "postcss-loader",
            options: { sourceMap: true },
          },
        ],
      },

      // Assets (images, fonts, etc.)
      {
        test: /\.(png|jpg|gif|eot|ttf|woff|woff2)$/,
        type: "asset",
        parser: {
          dataUrlCondition: {
            maxSize: 10000,
          },
        },
        generator: {
          filename: "static/media/[name].[hash:8][ext]",
        },
      },

      // SVG assets
      {
        test: /\.svg$/,
        type: "asset/resource",
        generator: {
          filename: "static/media/[name].[hash:8][ext]",
        },
      },
    ],
  },
  plugins: [
    // Replace ajv schema $ref pointers with empty module to prevent resolution errors
    // This is a workaround for https://github.com/EOSIO/eosjs/issues/1063
    new webpack.NormalModuleReplacementPlugin(
      /^\.\.\/\.\.\/WebpackOptions\.json$/,
      path.resolve(__dirname, "empty-module.js"),
    ),
    // Generates an `index.html` file with the <script> injected.
    new HtmlWebpackPlugin({
      inject: true,
      template: paths.appHtml,
    }),
    // Makes some environment variables available in index.html.
    // The public URL is available as %PUBLIC_URL% in index.html, e.g.:
    // <link rel="shortcut icon" href="%PUBLIC_URL%/favicon.ico">
    // In development, this will be an empty string.
    new InterpolateHtmlPlugin(HtmlWebpackPlugin, env.raw),
    // Makes some environment variables available to the JS code, for example:
    // if (process.env.NODE_ENV === 'development') { ... }. See `./env.js`.
    new webpack.DefinePlugin(env.stringified),
    // Provide process global for browser (Webpack 5 doesn't auto-polyfill)
    new webpack.ProvidePlugin({
      process: "process/browser",
    }),
    // This is necessary to emit hot updates (currently CSS only):
    new webpack.HotModuleReplacementPlugin(),
    // Watcher doesn't work well if you mistype casing in a path so we use
    // a plugin that prints an error when you attempt to do this.
    // See https://github.com/facebook/create-react-app/issues/240
    new CaseSensitivePathsPlugin(),
    // Inject the current git commit hash as `process.env.COMMIT`. We use this
    // to display version information
    new GitHashWebpackPlugin({ webpack, len: 8 }),
    // Filter out ajv schema resolution errors that don't affect runtime
    // This is a workaround for crypto-browserify/sjcl with Webpack 5
    {
      apply: (compiler) => {
        compiler.hooks.compilation.tap("FilterAjvErrors", (compilation) => {
          compilation.hooks.afterSeal.tap("FilterAjvErrors", () => {
            compilation.errors = compilation.errors.filter((error) => {
              const errorString = error.toString();
              const errorMessage = error.message || "";
              // Filter out WebpackOptions.json reference errors from crypto packages
              const isAjvError =
                (errorString.includes("can't resolve reference") ||
                  errorMessage.includes("can't resolve reference")) &&
                (errorString.includes("WebpackOptions.json") ||
                  errorMessage.includes("WebpackOptions.json"));
              const isCryptoPackage =
                errorString.includes("browserify-") ||
                errorString.includes("crypto-") ||
                errorString.includes("parse-asn1") ||
                errorString.includes("diffie-hellman") ||
                errorString.includes("elliptic") ||
                errorString.includes("ecurve") ||
                errorString.includes("bigi") ||
                errorMessage.includes("browserify-") ||
                errorMessage.includes("crypto-") ||
                errorMessage.includes("parse-asn1") ||
                errorMessage.includes("diffie-hellman") ||
                errorMessage.includes("elliptic") ||
                errorMessage.includes("ecurve") ||
                errorMessage.includes("bigi");

              if (isAjvError && isCryptoPackage) {
                return false; // Filter out this error
              }
              return true; // Keep other errors
            });
          });
        });
      },
    },
  ],

  // Turn off performance processing because we utilize
  // our own hints via the FileSizeReporter
  performance: false,

  // Ignore ajv schema resolution errors (they don't affect runtime)
  // This is a workaround for crypto-browserify and sjcl having old dependencies with Webpack 5
  ignoreWarnings: [
    {
      module:
        /node_modules\/(browserify-|crypto-|parse-|diffie-|elliptic|ecurve|bigi)/,
      message: /can't resolve reference.*WebpackOptions\.json/,
    },
  ],

  stats: {
    warningsFilter: /can't resolve reference.*WebpackOptions\.json/,
  },
};

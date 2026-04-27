"use strict";

const path = require("path");
const webpack = require("webpack");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const { WebpackManifestPlugin } = require("webpack-manifest-plugin");
const InterpolateHtmlPlugin = require("react-dev-utils/InterpolateHtmlPlugin");
const paths = require("../config/paths");
const getClientEnvironment = require("./env");
const CopyWebpackPlugin = require("copy-webpack-plugin");
const GitHashWebpackPlugin = require("git-hash-webpack-plugin");

// Webpack uses `publicPath` to determine where the app is being served from.
// It requires a trailing slash, or the file assets will get an incorrect path.
const publicPath = paths.servedPath;
// Source maps are resource heavy and can cause out of memory issue for large source files.
const shouldUseSourceMap = process.env.GENERATE_SOURCEMAP !== "false";
// `publicUrl` is just like `publicPath`, but we will provide it to our app
// as %PUBLIC_URL% in `index.html` and `process.env.PUBLIC_URL` in JavaScript.
// Omit trailing slash as %PUBLIC_URL%/xyz looks better than %PUBLIC_URL%xyz.
const publicUrl = publicPath.slice(0, -1);
// Get environment variables to inject into our app.
const env = getClientEnvironment(publicUrl);

const useDebugger = process.env.ELM_DEBUGGER === "true";

// This is the production configuration.
// It compiles slowly and is focused on producing a fast and minimal bundle.
// The development configuration is different and lives in a separate file.
module.exports = {
  mode: "production",
  // Don't attempt to continue if there are any errors.
  bail: true,
  // Disable webpack caching due to Node 24 compatibility issues with babel-loader
  cache: false,
  // We generate sourcemaps in production. This is slow but gives good results.
  // You can exclude the *.map files from the build during deployment.
  devtool: shouldUseSourceMap ? "source-map" : false,
  // In production, we only want to load the polyfills and the app code.
  entry: [require.resolve("./polyfills"), paths.appIndexJs],
  output: {
    // The build folder.
    path: paths.appBuild,
    // Generated JS file names (with nested folders).
    // There will be one main bundle, and one file per asynchronous chunk.
    // We don't currently advertise code splitting but Webpack supports it.
    filename: "static/js/[name].[contenthash:8].js",
    chunkFilename: "static/js/[name].[contenthash:8].chunk.js",
    // We inferred the "public path" (such as / or /my-project) from homepage.
    publicPath: publicPath,
    // Point sourcemap entries to original disk location (format as URL on Windows)
    devtoolModuleFilenameTemplate: (info) =>
      path
        .relative(paths.appSrc, info.absoluteResourcePath)
        .replace(/\\/g, "/"),
  },
  optimization: {
    // Webpack 5 uses TerserPlugin by default when mode is 'production'
    // It automatically handles ES6+ syntax and provides better compression than UglifyJS
    // Automatically split vendor and commons
    // https://twitter.com/wSokra/status/969633336732905474
    splitChunks: {
      chunks: "all",
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
    // Disabled strictExportPresence to allow eosjs/ajv to work despite schema resolution issues
    strictExportPresence: false,

    noParse: /\.elm$/,

    rules: [
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
        loader: require.resolve("babel-loader"),
        options: {
          // Latest stable ECMAScript features
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
        exclude: [/[/\\\\]elm-stuff[/\\\\]/, /[/\\\\]node_modules[/\\\\]/],
        use: [
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
            // Use the local installation of elm make
            loader: require.resolve("elm-webpack-loader"),
            options: {
              // If ELM_DEBUGGER was set to "true", enable it. Otherwise
              // for invalid values, "false" and as a default, disable it
              debug: useDebugger,
              optimize: !useDebugger,
              pathToElm: paths.elm,
            },
          },
        ],
      },

      // "postcss" loader applies autoprefixer to our CSS.
      // "css" loader resolves paths in CSS and adds assets as dependencies.
      // `MiniCSSExtractPlugin` extracts styles into CSS
      // files. If you use code splitting, async bundles will have their own separate CSS chunk file.
      // By default we support CSS Modules with the extension .module.css
      {
        test: /\.css$/,
        exclude: /\.module\.css$/,
        use: [
          MiniCssExtractPlugin.loader,
          {
            loader: require.resolve("css-loader"),
            options: {
              importLoaders: 1,
              sourceMap: shouldUseSourceMap,
              url: {
                filter: (url) => !url.startsWith("/"),
              },
            },
          },
          {
            loader: require.resolve("postcss-loader"),
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
      minify: {
        removeComments: true,
        collapseWhitespace: true,
        removeRedundantAttributes: true,
        useShortDoctype: true,
        removeEmptyAttributes: true,
        removeStyleLinkTypeAttributes: true,
        keepClosingSlash: true,
        minifyJS: true,
        minifyCSS: true,
        minifyURLs: true,
      },
    }),
    // Makes some environment variables available in index.html.
    // The public URL is available as %PUBLIC_URL% in index.html, e.g.:
    // <link rel="shortcut icon" href="%PUBLIC_URL%/favicon.ico">
    // In production, it will be an empty string unless you specify "homepage"
    // in `package.json`, in which case it will be the pathname of that URL.
    new InterpolateHtmlPlugin(HtmlWebpackPlugin, env.raw),
    // Makes some environment variables available to the JS code, for example:
    // if (process.env.NODE_ENV === 'production') { ... }. See `./env.js`.
    // It is absolutely essential that NODE_ENV was set to production here.
    // Otherwise React will be compiled in the very slow development mode.
    new webpack.DefinePlugin(env.stringified),
    // Provide process global for browser (Webpack 5 doesn't auto-polyfill)
    new webpack.ProvidePlugin({
      process: "process/browser",
    }),
    // Note: this won't work without ExtractTextPlugin.extract(..) in `loaders`.
    new MiniCssExtractPlugin({
      // Options similar to the same options in webpackOptions.output
      // both options are optional
      filename: "static/css/[name].[contenthash:8].css",
      chunkFilename: "static/css/[name].[contenthash:8].chunk.css",
    }),
    // Generate a manifest file which contains a mapping of all asset filenames
    // to their corresponding output file so that tools can pick it up without
    // having to parse `index.html`.
    new WebpackManifestPlugin({
      fileName: "asset-manifest.json",
      publicPath: publicPath,
    }),
    // Copy our service worker file to the ROOT of the build folder
    // Exclude index.html since HtmlWebpackPlugin generates it
    new CopyWebpackPlugin({
      patterns: [
        {
          from: "public/",
          to: "",
          globOptions: {
            ignore: ["**/index.html"],
          },
        },
      ],
    }),
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

  // Ignore known issues with crypto-browserify/sjcl and ajv schema resolution
  // These don't affect runtime functionality
  ignoreWarnings: [
    {
      module:
        /node_modules\/(browserify-|crypto-|parse-|diffie-|elliptic|ecurve|bigi)/,
      message: /can't resolve reference.*WebpackOptions\.json/,
    },
  ],
};

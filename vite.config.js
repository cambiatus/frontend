import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";
import { nodePolyfills } from "vite-plugin-node-polyfills";
import { execSync } from "child_process";

// Custom plugin to inject git hash
const gitHashPlugin = () => ({
  name: "git-hash",
  config: () => ({
    define: {
      "process.env.COMMIT": JSON.stringify(
        execSync("git rev-parse --short HEAD").toString().trim(),
      ),
    },
  }),
});

export default defineConfig({
  plugins: [
    elmPlugin({
      debug: false,
      optimize: process.env.NODE_ENV === "production",
    }),
    nodePolyfills({
      // Enable polyfills for eosjs crypto dependencies
      include: [
        "crypto", // crypto-browserify
        "stream", // stream-browserify
        "util", // util
        "buffer", // buffer
        "process", // process/browser
        "url", // ensure url + punycode are bundled for node-stdlib-browser
      ],
      globals: {
        Buffer: true,
        global: true,
        process: true,
      },
    }),
    gitHashPlugin(),
  ],

  build: {
    outDir: "dist",
    sourcemap: true,
    // Increase chunk size warning limit for Elm apps
    chunkSizeWarningLimit: 1000,
    // Inline small assets as base64
    assetsInlineLimit: 4096,
    rollupOptions: {
      output: {
        // Manual code splitting for better caching
        manualChunks: {
          "vendor-blockchain": [
            "eosjs",
            "eosjs/dist/eosjs-jssig",
            "eosjs/dist/eosjs-ecc-migration",
          ],
          "vendor-graphql": ["phoenix", "@absinthe/socket"],
          "vendor-ui": ["quill", "pdfjs-dist", "@paypal/paypal-js", "pdfmake"],
        },
      },
    },
  },

  server: {
    port: 3000,
    open: true,
    // Enable HMR
    hmr: {
      overlay: true,
    },
  },

  define: {
    "process.env.NODE_ENV": JSON.stringify(
      process.env.NODE_ENV || "development",
    ),
    // USE_SUBDOMAIN is used in src/index.js
    "process.env.USE_SUBDOMAIN": JSON.stringify(process.env.USE_SUBDOMAIN),
  },

  // Optimize dependencies
  optimizeDeps: {
    include: [
      "eosjs",
      "phoenix",
      "@absinthe/socket",
      "quill",
      "pdfmake",
      "@paypal/paypal-js",
      "bip39",
      "sjcl",
    ],
  },

  // Public directory for static assets
  publicDir: "public",

  // Resolve configuration
  resolve: {
    alias: {
      // Ensure proper resolution of node-fetch for eosjs
      "node-fetch": "node-fetch",
      // Some dependencies (url polyfill) expect punycode next to the package
      // tree; point explicitly to the hoisted copy to avoid ENOENT errors.
      punycode: "punycode/",
    },
  },
});

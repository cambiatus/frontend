# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Cambiatus is an Elm-based frontend for a community currency and exchange platform. It uses GraphQL for API communication, Tailwind CSS for styling, and Vite for bundling. The app supports multiple languages via i18next and integrates with EOSIO blockchain functionality.

## Quick Start Commands

- **Development**: `yarn start` - Starts Vite dev server with HMR (Hot Module Replacement)
- **Build**: `yarn build` - Builds production bundle with Vite
- **Preview**: `yarn preview` - Preview production build locally
- **Type checking (Elm)**: `yarn check-format` - Validates Elm code formatting
- **Linting (Elm)**: `yarn review` - Runs elm-review for code quality checks
- **Linting (JS)**: `yarn check-js` - Runs StandardJS linter on JavaScript
- **CSS linting**: `yarn check-css` - Validates CSS with StyleLint
- **Tests**: `yarn test` - Runs elm-test suite
- **Code generation**: `yarn generate-graphql` - Regenerates Elm code from GraphQL schema (targets staging environment)
- **Format check**: `yarn check-format` - Validates elm-format compliance
- **Pre-push checks**: Husky runs all linters and tests before push

## Architecture

### Entry Point and Routing
- **Main.elm**: Application entry point. Manages root-level routing and session initialization
- **Page.elm**: Handles session management (Guest vs LoggedIn) and page-level initialization
- **Route.elm**: Defines all application routes and URL parsing

### Session Management
The app has two session types:
- **Session.Guest**: For unauthenticated users (login, register, join)
- **Session.LoggedIn**: For authenticated users, manages user data, communities, and notifications
- **Session.Shared**: Common data across both sessions (language, environment, translations, current time/timezone)

### Page Structure
Pages are organized in `src/elm/Page/` with subdirectories for feature areas:
- `Page/Community/` - Community management pages (settings, objectives, sponsorship, transfers)
- `Page/Dashboard.elm` - Main user dashboard with claims and analysis
- `Page/Profile/` - User profile and related features (KYC, contacts, claims)
- `Page/Shop.elm` - Commerce functionality
- `Page/Register/` - Registration flow
- `Page/Login.elm` - Authentication page
- `Page/Settings.elm` - User settings

### Core Modules
- **Action.elm**: Represents user actions/claims in the community (large file, handles claim logic)
- **Community.elm**: Community data structures and management
- **Claim.elm**: Claim verification and processing logic
- **Eos.elm**: EOSIO blockchain integration
- **Api/Graphql.elm**: GraphQL query/mutation helpers and configuration
- **Form.elm**: Shared form component logic (large file for handling form state)
- **Icons.elm**: SVG icon definitions (large file)
- **Markdown.elm**: Markdown rendering utilities
- **Log.elm**: Breadcrumb and logging utilities

### API and Data
- **src/elm/Api/Graphql/**: Auto-generated GraphQL types and operations (do not edit - regenerated from schema)
- **src/elm/Cambiatus/**: GraphQL-generated types and scalars
- **Graphql operations**: Use `Api.Graphql` module which provides query/mutation helpers with Relay pagination support

### Styling
- **src/styles/**: CSS files with Tailwind CSS integration
- Builds with PostCSS, Autoprefixer, and Tailwind
- **tailwind.config.js**: Tailwind configuration in root
- **Stylelint** validates CSS compliance

### JavaScript/Custom Elements
- **src/scripts/config.js**: Environment configuration (loaded by Vite)
- **src/scripts/registerServiceWorker.js**: Service worker registration
- **src/customElements/**: Custom HTML elements and JavaScript utilities
- **src/utils/**: JavaScript utility functions
- StandardJS linter enforces JS style

### Build System: Vite

The app uses Vite 2.9+ with vite-plugin-elm for fast, modern builds.

**Configuration:** `vite.config.js` in root
**Entry point:** `/index.html` (loads `/src/index.js` as ES module)
**Public assets:** `/public/` (copied automatically to `/dist/`)

**Key features:**
- Fast HMR for Elm and CSS (typically <1s)
- Node.js polyfills for eosjs (crypto, buffer, stream, util, process)
- Automatic code splitting and tree shaking
- PostCSS + Tailwind CSS v2 (JIT mode)
- Git hash injection for versioning
- Manual chunks: vendor-blockchain, vendor-graphql, vendor-ui

**Legacy Webpack:**
- Webpack config files preserved in `/config/` for rollback
- Use `yarn start:webpack` or `yarn bundle:webpack` if needed

### Testing
- **tests/**: Elm test files using `elm-explorations/test`
- Tests cover utilities, business logic, and API parsing
- Run with `yarn test` or `elm-test`

## Configuration

The app uses a multi-layered configuration system:
1. **src/scripts/config.js**: Default configuration for DEV and PROD environments
2. **env-config.js**: Environment-specific overrides (dev/prod/demo servers)
3. **src/elm/Flags.elm**: Elm configuration with JSON decoding and defaults (loaded from config.js via port)

During development, the local environment defaults to staging server without loading env-config.js.

## Important Patterns

### Update Pattern
Uses `UpdateResult` (UR) for composable command handling:
```elm
UR.init model
  |> UR.addCmd cmd
  |> UR.map identity GotMsg (\_ ur -> ur)
```

### Remote Data
Uses `RemoteData` for API request states (Loading, Success, Failure)

### I18n
Translations loaded asynchronously in `Page.init`. Use `I18Next` module for accessing translations in components.

### GraphQL Generation
Run `yarn generate-graphql` to regenerate Elm types from GraphQL schema. This targets the staging API endpoint. Output goes to `src/elm/Cambiatus/`.

## Code Quality

- **Elm**: Use `elm-format`, `elm-review`, and `elm-test`
- **JavaScript**: StandardJS for consistency (ES5 target in Babel)
- **CSS**: StyleLint with standard configuration
- **Pre-push hook**: Validates all checks before allowing push
- Manual code review required before merge per CONTRIBUTING.md

## Common Development Tasks

### Adding a new page
1. Create module in `src/elm/Page/`
2. Add route in `Route.elm`
3. Handle in `Page.init` and `Page.update`
4. Add to `Main.elm` view mapping

### Adding GraphQL queries
1. Ensure schema is up to date
2. Run `yarn generate-graphql` to regenerate types
3. Use generated types in `Api.Graphql` module

### Debugging
- **Elm errors**: Check console; elm-test for logic errors
- **Graphql errors**: Check Network tab and API responses
- **Styling**: Use browser DevTools; check Tailwind class application
- **Logging**: Use `Log` module for breadcrumbs (check console)

## Dependencies

### Key Elm packages
- `dillonkearns/elm-graphql`: GraphQL code generation and queries
- `krisajenkins/remotedata`: Request state management
- `elm/browser`: Single-page app framework
- `elm-community/list-extra`, `string-extra`: Utility extensions
- `rtfeldman/elm-validate`: Validation logic
- `dillonkearns/elm-markdown`: Markdown rendering

### Key JavaScript packages
- `vite`, `vite-plugin-elm`: Bundling and dev server with Elm support
- `vite-plugin-node-polyfills`: Node.js polyfills for browser (eosjs compatibility)
- `@tailwindcss/*`: Tailwind CSS plugins
- `@sentry/browser`: Error reporting
- `phoenix`: WebSocket support
- `eosjs`: EOSIO blockchain integration (v22)
- `@absinthe/socket`: GraphQL subscriptions over Phoenix WebSocket

## File Reading Tips

**Large files** (1000+ lines):
- `src/elm/Main.elm`: Main app orchestration
- `src/elm/Action.elm`: Action/claim business logic
- `src/elm/Form.elm`: Form handling utilities
- `src/elm/Icons.elm`: Icon definitions
- `src/elm/Session/LoggedIn.elm`: Authenticated user session
- `src/elm/Page/Dashboard.elm`: Dashboard page implementation

Focus on specific functions or sections rather than reading entire files.

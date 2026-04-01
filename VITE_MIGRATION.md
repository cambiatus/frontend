# Cambiatus Frontend Modernization Plan: Webpack → Vite Migration

## Executive Summary

**Recommendation:** Migrate from Webpack 5 to Vite 2.9+ using the proven elm-book configuration as a blueprint.

**Why Vite?**
- ✅ **Already working in this project** - elm-book workspace successfully uses Vite 2.9.16
- ✅ **Solves current Webpack issues** - Eliminates ajv schema errors, HMR problems, and complex polyfill workarounds
- ✅ **Industry standard for modern Elm** - Vite is the go-to build tool for Elm projects in 2024-2025
- ✅ **Significantly better DX** - 5-6x faster dev server, 3-5x faster hot reload, 2-3x faster production builds
- ✅ **Simpler configuration** - elm-book's vite.config.js is 7 lines vs 300+ lines of webpack config

**Estimated Effort:** 2-3 days of focused work

**Risk Level:** Low-Medium (proven approach with rollback plan)

---

## Current State Analysis

### Problems with Webpack 5 Setup (from MODERNIZE.md)
1. **ajv schema resolution errors** - 10+ errors from crypto libraries (crypto-browserify, elliptic, eosjs)
2. **WebSocket/HMR failures** - Dev server connectivity issues
3. **Complex workarounds** - NormalModuleReplacementPlugin, custom error filtering, extensive polyfills
4. **Runtime errors** - Package.json resolution, syntax errors from websocket endpoints
5. **Maintenance burden** - 300+ lines of configuration across dev/prod configs

### What Works (Must Preserve)
- 30+ Elm-JavaScript port operations
- 14 custom web components (PayPal, PDF viewer, rich text editor, etc.)
- eosjs v22 blockchain integration
- GraphQL subscriptions via Absinthe Socket + Phoenix WebSocket
- Multi-layer environment configuration (env-config.js runtime loading)
- Tailwind CSS v2.2.19 with JIT mode
- Service worker with precaching
- Sentry error tracking

---

## Migration Strategy

### Approach: Phased Big Bang

**Not** a gradual migration - running two build systems creates unnecessary complexity. Instead:
1. Create migration branch from `2025-modernization`
2. Complete migration in phases with testing checkpoints
3. Merge when fully validated
4. Keep Webpack config in git history for easy rollback if needed

### Timeline

| Phase | Duration | Focus |
|-------|----------|-------|
| **Phase 1: Foundation** | 4-6 hours | Dependencies, config files, HTML entry |
| **Phase 2: Polyfills & Integration** | 3-4 hours | Node polyfills, custom plugins, env vars |
| **Phase 3: Testing & Debugging** | 6-8 hours | All critical flows, WebSocket, blockchain |
| **Phase 4: Production Build** | 2-3 hours | Optimization, asset copying, manifest |
| **Phase 5: Documentation** | 1-2 hours | Update MODERNIZE.md, README, scripts |

**Total:** 16-23 hours (~2-3 days)

---

## Implementation Plan

### Phase 1: Foundation Setup

#### 1.1 Install Vite Dependencies

**Add to package.json:**
```json
{
  "devDependencies": {
    "vite": "^2.9.16",
    "vite-plugin-elm": "^2.5.0",
    "vite-plugin-node-polyfills": "^0.7.0"
  }
}
```

**Remove (not immediately - keep for rollback):**
- Keep webpack dependencies until migration is validated
- Can remove after successful production deployment

#### 1.2 Create vite.config.js

**File:** `/vite.config.js`

**Blueprint from elm-book** (expand with production needs):
```javascript
import { defineConfig } from 'vite'
import elmPlugin from 'vite-plugin-elm'
import { nodePolyfills } from 'vite-plugin-node-polyfills'

export default defineConfig({
  plugins: [
    elmPlugin(),
    nodePolyfills({
      // Enable polyfills for eosjs crypto dependencies
      include: ['crypto', 'stream', 'util', 'buffer', 'process']
    })
  ],
  
  build: {
    outDir: 'dist',
    sourcemap: true,
    rollupOptions: {
      output: {
        manualChunks: {
          vendor: ['eosjs', 'phoenix', '@absinthe/socket']
        }
      }
    }
  },
  
  server: {
    port: 3000,
    open: true
  },
  
  define: {
    'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV)
  }
})
```

**Estimated:** 150-200 lines total (vs 300+ in webpack configs)

#### 1.3 Move HTML Entry Point

**Action:** Move `/public/index.html` → `/index.html` (Vite requirement)

**Key changes in index.html:**
```html
<!-- Remove %PUBLIC_URL% placeholders - Vite handles this differently -->
<!-- Before: -->
<script src="%PUBLIC_URL%/env-config.js"></script>
<!-- After: -->
<script src="/env-config.js"></script>

<!-- Add module script for Vite entry -->
<!-- Add at end of <body>: -->
<script type="module" src="/src/index.js"></script>
```

**Files to update:**
- `/index.html` - New location
- Keep `/public/env-config.js` - Still loaded at runtime
- Update asset paths to remove `%PUBLIC_URL%` (Vite uses absolute `/` paths)

#### 1.4 Update package.json Scripts

**Replace:**
```json
{
  "scripts": {
    "start": "vite",
    "build": "vite build",
    "preview": "vite preview",
    "bundle": "vite build",
    
    // Keep for transition period
    "start:webpack": "node scripts/start.js",
    "bundle:webpack": "NODE_ENV=production webpack --mode=production"
  }
}
```

### Phase 2: Polyfills & Integration

#### 2.1 Node.js Polyfills for eosjs

**Plugin:** `vite-plugin-node-polyfills` (already in elm-book dependencies)

**Configuration in vite.config.js:**
```javascript
nodePolyfills({
  include: [
    'crypto',    // crypto-browserify
    'stream',    // stream-browserify  
    'util',      // util
    'buffer',    // buffer
    'process'    // process/browser
  ],
  globals: {
    Buffer: true,
    global: true,
    process: true
  }
})
```

**Why this works:** Vite's plugin handles polyfills cleanly without webpack's NormalModuleReplacementPlugin hacks.

#### 2.2 Environment Variables

**Current system (preserve):**
- `public/env-config.js` - Runtime configuration (loaded via `<script>` tag)
- Sets `window._env_` object
- No changes needed - works with Vite

**Vite environment variables:**
```javascript
// In vite.config.js
define: {
  'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV),
  // Add others if needed for build-time access
}
```

**In src/index.js - No changes needed:**
- Already uses `window._env_` for runtime config
- Build-time vars accessed via `process.env` will work with `define`

#### 2.3 Git Hash Injection

**Replace:** `git-hash-webpack-plugin`

**Vite approach - Custom plugin:**
```javascript
// In vite.config.js
import { execSync } from 'child_process'

const gitHashPlugin = () => ({
  name: 'git-hash',
  config: () => ({
    define: {
      'process.env.COMMIT': JSON.stringify(
        execSync('git rev-parse --short HEAD').toString().trim()
      )
    }
  })
})

// Add to plugins array
plugins: [elmPlugin(), nodePolyfills(), gitHashPlugin()]
```

#### 2.4 Asset Copying

**Vite handles public/ automatically** - No plugin needed!

- All files in `/public/` are copied to `/dist/` automatically
- Keep PDF worker postinstall script: `cp ./node_modules/pdfjs-dist/build/pdf.worker.min.mjs ./public/pdf.worker.min.js`

**Exception - env-config.js:**
- Already in `/public/` ✅
- Will be copied automatically ✅

### Phase 3: Testing & Validation

#### 3.1 Development Server Testing

**Start dev server:**
```bash
yarn start  # Now runs: vite
```

**Test checklist:**
- [ ] App loads without errors
- [ ] Elm app initializes with flags
- [ ] Hot reload works for Elm changes
- [ ] Hot reload works for CSS changes
- [ ] Custom elements register correctly
- [ ] env-config.js loads before app init
- [ ] No WebSocket/HMR errors in console
- [ ] Port operations work (test login, key generation)

#### 3.2 Critical Integration Testing

**High priority flows:**
1. **Authentication**
   - Login with existing account
   - Generate new keys
   - Sign string with private key
   - PIN encryption/decryption

2. **Blockchain (eosjs)**
   - Account name validation
   - EOS transaction signing
   - RPC calls to chain
   - Verify crypto polyfills work (no `crypto` undefined errors)

3. **GraphQL Subscriptions**
   - Absinthe Socket connection
   - Phoenix WebSocket stability
   - Real-time updates (transfers, notifications)

4. **Custom Elements**
   - PDF viewer (pdfjs-dist)
   - Rich text editor (Quill)
   - PayPal buttons integration
   - Image cropper
   - Date formatter

5. **Styling**
   - Tailwind classes apply correctly
   - JIT mode works
   - PostCSS transformations
   - Custom animations

#### 3.3 Production Build Testing

**Build command:**
```bash
yarn build  # Now runs: vite build
```

**Validation checklist:**
- [ ] Build completes without errors
- [ ] No ajv schema warnings (should be gone!)
- [ ] Output in `/dist/` directory
- [ ] Assets have content hashes (cache busting)
- [ ] Source maps generated
- [ ] Bundle size reasonable (compare to webpack)
- [ ] All public assets copied
- [ ] env-config.js in dist root

**Local production testing:**
```bash
yarn preview  # Vite's production preview server
```

Test same flows as development but from production build.

### Phase 4: Production Optimization

#### 4.1 Code Splitting

**Vite does this automatically**, but we can optimize:

```javascript
// In vite.config.js
build: {
  rollupOptions: {
    output: {
      manualChunks: {
        'vendor-blockchain': ['eosjs'],
        'vendor-graphql': ['phoenix', '@absinthe/socket'],
        'vendor-ui': ['quill', 'pdfjs-dist', '@paypal/paypal-js']
      }
    }
  }
}
```

**Benefits:**
- Better caching (vendor chunks change less frequently)
- Parallel loading
- Smaller initial bundle

#### 4.2 Asset Optimization

**Images:**
```javascript
// In vite.config.js
build: {
  assetsInlineLimit: 4096, // Inline small assets as base64
}
```

**CSS:**
- Vite automatically minifies and extracts CSS
- PostCSS pipeline preserved (Tailwind, autoprefixer)

#### 4.3 Legacy Browser Support (Optional)

**Current Babel target:** ES5

**Vite approach:**
```javascript
// In vite.config.js
import legacy from '@vitejs/plugin-legacy'

plugins: [
  legacy({
    targets: ['defaults', 'not IE 11']
  })
]
```

**Decision point:** Check browser analytics - do you need ES5? Most users likely on modern browsers.

### Phase 5: Documentation & Cleanup

#### 5.1 Update MODERNIZE.md

**Add section:**
```markdown
## ✅ COMPLETED: Migration to Vite

**Date:** [Date]
**Status:** SUCCESS

### Changes Made
- Migrated from Webpack 5 to Vite 2.9
- Eliminated ajv schema resolution errors
- Simplified build configuration (300+ lines → ~150 lines)
- Improved dev server performance (5-6x faster startup)

### Issues Resolved
- ✅ ajv schema errors from crypto libraries
- ✅ WebSocket/HMR connection failures  
- ✅ Package.json resolution issues
- ✅ Complex polyfill workarounds

### Performance Improvements
- Dev server startup: 30-60s → 5-10s
- Hot reload: 3-5s → <1s
- Production build: 2-3min → 30-60s

### Configuration Files
- Added: vite.config.js
- Moved: public/index.html → index.html
- Updated: package.json scripts
- Deprecated: config/webpack.config.*.js (kept in git history)
```

#### 5.2 Update CLAUDE.md

**Quick Start Commands section:**
```markdown
- **Development**: `yarn start` - Starts Vite dev server with HMR
- **Build**: `yarn build` - Builds production bundle with Vite
- **Preview**: `yarn preview` - Preview production build locally
```

**Build System section:**
```markdown
### Build System: Vite

The app uses Vite 2.9+ with vite-plugin-elm for fast, modern builds.

**Configuration:** vite.config.js in root
**Entry point:** /index.html (loads /src/index.js as ES module)
**Public assets:** /public/ (copied automatically to /dist/)

**Key features:**
- Fast HMR for Elm and CSS
- Node.js polyfills for eosjs (crypto, buffer, stream, util, process)
- Automatic code splitting and tree shaking
- PostCSS + Tailwind CSS v2 (JIT mode)
- Git hash injection for versioning
```

#### 5.3 Clean Up (Optional - Post-Validation)

**After 1-2 weeks of successful production use:**

**Remove from package.json:**
- webpack
- webpack-dev-server
- webpack-cli
- webpack-manifest-plugin
- All webpack-specific loaders (babel-loader, elm-webpack-loader, etc.)
- Plugin dependencies (html-webpack-plugin, mini-css-extract-plugin, etc.)

**Remove files:**
- `config/webpack.config.dev.js`
- `config/webpack.config.prod.js`
- `scripts/start.js` (webpack dev server script)
- `scripts/utils/webpackHotDevClient.js`

**Keep in git history** - Don't squash commits, makes rollback easier.

---

## Rollback Plan

### If Critical Issues Arise

**Before removing webpack dependencies:**

```bash
# Revert package.json scripts
git checkout HEAD~1 package.json

# Restore webpack as start command
yarn start:webpack

# Or revert entire branch
git checkout 2025-modernization
git branch -D vite-migration
```

**After removing webpack dependencies:**

```bash
# Revert to last webpack commit
git revert HEAD~5..HEAD  # Or specific range
yarn install
```

### Known Fallback Points

1. **WebSocket issues** → Test with webpack-dev-server config in Vite (proxy settings)
2. **Custom element loading** → Ensure Vite doesn't split these into async chunks
3. **eosjs crypto errors** → Verify polyfill plugin configuration
4. **Service worker** → May need vite-plugin-pwa for advanced precaching

---

## Critical Files Reference

### Files to Create
- `/vite.config.js` - Main Vite configuration (~150-200 lines)
- `/index.html` - Move from /public/index.html, update script tags

### Files to Modify
- `/package.json` - Update scripts, add Vite dependencies
- `/src/index.js` - Minimal changes (verify env var access)
- `/MODERNIZE.md` - Document successful migration
- `/CLAUDE.md` - Update build system documentation

### Files to Keep (No Changes)
- `/public/env-config.js` - Runtime configuration
- `/src/elm/**/*.elm` - Elm source code
- `/src/customElements/**/*.js` - Custom web components
- `/tailwind.config.js` - Tailwind configuration
- `/postcss.config.js` - PostCSS configuration
- `/elm.json` - Elm package configuration
- All asset files in `/public/`

### Files to Preserve (For Rollback)
- `/config/webpack.config.dev.js`
- `/config/webpack.config.prod.js`
- `/scripts/start.js`

---

## Success Criteria

### Development
- [ ] `yarn start` completes in <10 seconds
- [ ] Hot reload for Elm changes in <1 second
- [ ] No console errors on app initialization
- [ ] All 14 custom elements register successfully
- [ ] Port operations work bidirectionally
- [ ] WebSocket connections stable

### Production
- [ ] `yarn build` completes in <60 seconds
- [ ] No build errors or warnings (except optional ones)
- [ ] Bundle size comparable or smaller than webpack
- [ ] All assets copied to /dist/
- [ ] Source maps generated
- [ ] Production preview (`yarn preview`) works

### Integration
- [ ] Login/authentication flows work
- [ ] EOS blockchain transactions succeed
- [ ] GraphQL subscriptions receive real-time updates
- [ ] PDF viewer displays documents
- [ ] Rich text editor saves content
- [ ] PayPal integration processes payments
- [ ] Sentry error tracking captures events

### Quality
- [ ] No regression in functionality
- [ ] Performance improved (faster dev server, builds)
- [ ] Configuration simpler and more maintainable
- [ ] Documentation updated

---

## Alternative Considered (Not Recommended)

### Finish Webpack 5 Migration

**Why not:**
- Webpack 5 issues are inherent to ecosystem (ajv schemas, crypto libraries)
- More complex configuration
- Slower performance
- Not the modern Elm standard
- elm-book already proves Vite works

**When to consider:** If Vite migration fails and rollback is needed.

---

## Notes & Considerations

### Learnings from elm-book
- Minimal configuration works (7 lines!)
- vite-plugin-elm handles Elm compilation seamlessly
- Tailwind CSS v3 works (consider upgrading main app later)
- PostCSS pipeline preserved
- elm-tooling optional but recommended for team consistency

### Modern Elm Ecosystem (2024-2025)
- Vite is the de facto standard for new Elm projects
- Active community support (vite-plugin-elm maintained)
- Elm Pages v3 uses Vite internally
- Better ESM support than webpack

### Risk Mitigation
- Start with development environment only
- Test thoroughly before production build
- Keep webpack as fallback during transition
- Document any Vite-specific issues encountered
- Incremental testing after each phase

### Team Onboarding
- Update README with new commands
- Document Vite-specific debugging (different error messages)
- Share performance improvements to build buy-in
- Update CI/CD pipelines to use `yarn build` (Vite)

---

## Next Actions

1. **Create migration branch:** `git checkout -b vite-migration`
2. **Install dependencies:** `yarn add -D vite vite-plugin-elm vite-plugin-node-polyfills`
3. **Create vite.config.js** with basic configuration
4. **Move index.html** from public/ to root
5. **Update package.json** scripts
6. **Test development server:** `yarn start`
7. **Iterate and debug** until all features work
8. **Test production build:** `yarn build && yarn preview`
9. **Update documentation**
10. **Create PR for review**

**Estimated completion:** 2-3 days of focused work

**First checkpoint:** Development server running without errors (Day 1 goal)

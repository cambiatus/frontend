import * as bip39 from 'bip39'
import { sha256, PrivateKey } from 'eosjs/dist/eosjs-key-conversions'
import { KeyType, privateKeyToString } from 'eosjs/dist/eosjs-numeric'
import english from 'bip39/src/wordlists/english.json'
import portuguese from 'bip39/src/wordlists/portuguese.json'
import spanish from 'bip39/src/wordlists/spanish.json'

// bip39 loads its wordlist JSONs through try/catch require()s (see bip39
// src/_wordlists.js), which the production bundler drops — so bip39.wordlists.*
// is undefined in the browser. Import the JSONs directly so they're bundled.
const wordlists = { english, portuguese, spanish }

/***
 * Generates a random mnemonic
 * @param {string} userLocale - User's locale.
 * @returns {[string,string]}
 */
function generateRandom (userLocale) {
  let wordlist = wordlists.english
  if (userLocale.toLowerCase().startsWith('es')) {
    wordlist = wordlists.spanish
  } else if (userLocale.toLowerCase().startsWith('pt')) {
    wordlist = wordlists.portuguese
  }

  const strength = undefined
  const rng = undefined

  const mnemonic = bip39.generateMnemonic(strength, rng, wordlist)
  return [mnemonic, toSeedHex(mnemonic)]
}

/**
 * Transform mnemonic words to hex
 * @param {} mnemonic
 */
function toSeedHex (mnemonic) {
  return bip39.mnemonicToSeedSync(mnemonic).toString('hex')
}

/**
 * Derive an EOS private key from a seed string.
 *
 * Replaces ecc.seedPrivate, which is a no-op stub in eosjs-ecc-migration
 * (it only logs "Method deprecated" and returns undefined). Reproduces the
 * legacy eosjs-ecc behaviour exactly: PrivateKey(sha256(seed)), returned as a
 * legacy WIF string, so keys match what existing accounts registered with.
 * @param {string} seed
 * @returns {string} legacy WIF private key
 */
function seedPrivate (seed) {
  const data = Uint8Array.from(sha256(seed))
  return PrivateKey.fromString(privateKeyToString({ type: KeyType.k1, data })).toLegacyString()
}

// =========================================
// Exports
// =========================================

export default {
  generateRandom,
  toSeedHex,
  seedPrivate,
  wordlists,
}

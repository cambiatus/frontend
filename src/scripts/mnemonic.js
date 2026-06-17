import * as bip39 from 'bip39'
import { sha256, PrivateKey } from 'eosjs/dist/eosjs-key-conversions'
import { KeyType, privateKeyToString } from 'eosjs/dist/eosjs-numeric'

/***
 * Generates a random mnemonic
 * @param {string} userLocale - User's locale.
 * @returns {[string,string]}
 */
function generateRandom (userLocale) {
  let wordlist = bip39.wordlists.english
  if (userLocale.toLowerCase().startsWith('es')) {
    wordlist = bip39.wordlists.spanish
  } else if (userLocale.toLowerCase().startsWith('pt')) {
    wordlist = bip39.wordlists.portuguese
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
}

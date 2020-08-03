import bip39 from 'bip39'
import ecc from 'eosjs-ecc'
import scrypt from 'scrypt-async'

/***
 * Generates a mnemonic from a string
 * @param password
 */
function generateFrom (password) {
  return new Promise(resolve => {
    hash(password).then(hash => {
      const mnemonic = bip39.entropyToMnemonic(hash)

      resolve([mnemonic, bip39.mnemonicToSeedHex(mnemonic)])
    })
  })
}

/***
 * Generates a random mnemonic
 * @param {string} userLang - User's language.
 * @returns {[string,string]}
 */
function generateRandom (userLang) {
  const strength = null
  const rng = null
  const wordlist = (userLang === 'es') ? bip39.wordlists.spanish : null
  const mnemonic = bip39.generateMnemonic(strength, rng, wordlist)
  return [mnemonic, bip39.mnemonicToSeedHex(mnemonic)]
}

/**
 * Transform mnemonic words to hex
 * @param {} mnemonic
 */
function toSeedHex (mnemonic) {
  return bip39.mnemonicToSeedHex(mnemonic)
}

/**
 * Hashes a text using a random salt
 * @param {} text
 */
function hash (text) {
  hashWithSalt(text, newSalt())
}

/***
 * Hashes a text using salt
 * @param text
 * @returns {Promise(string)}
 */
function hashWithSalt (text, salt) {
  return new Promise(async resolve => {
    // We don't need a salt here, because this is a non-saved(!) hash,
    // which is used to create a seed that is used to encrypt
    // the keychain using AES which has it's own salt.
    scrypt(
      text,
      salt,
      {
        N: 16384,
        r: 8,
        p: 1,
        dkLen: 16,
        encoding: 'hex'
      },
      derivedKey => {
        resolve(derivedKey)
      }
    )
  })
}

// =========================================
// Generators
// =========================================

function newSalt () {
  return ecc.sha256(text(32))
}

function rand () {
  const arr = new Uint32Array(1)
  window.crypto.getRandomValues(arr)
  return arr[0] / (0xffffffff + 1)
}

/***
 * Generates a random string of specified size
 * @param size - The length of the string to generate
 * @returns {string} - The generated random string
 */
function text (length) {
  let text = ''
  const possible =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'
  for (let i = 0; i < length; i++) {
    text += possible.charAt(Math.floor(rand() * possible.length))
  }
  return text
}

// =========================================
// Exports
// =========================================

export default {
  generateFrom,
  generateRandom,
  toSeedHex,
  hash,
  hashWithSalt
}

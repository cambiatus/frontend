import * as bip39 from 'bip39'

/***
 * Generates a random mnemonic
 * @param {string} userLocale - User's locale.
 * @returns {[string,string]}
 */
function generateRandom (userLocale) {
  const userLang = userLocale.toLowerCase().split('-')[0]

  let wordlist = bip39.wordlists.english
  switch (userLang) {
    case 'es':
      wordlist = bip39.wordlists.spanish
      break;
    case 'pt':
      wordlist = bip39.wordlists.portuguese
      break;

    default:
      break;
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

// =========================================
// Exports
// =========================================

export default {
  generateRandom,
  toSeedHex,
}

/* global _env_ */
const local = {
  network: {
    blockchain: 'eos',
    protocol: 'http',
    host: 'localhost',
    port: 8888,
    chainId: 'cf057bbfb72640471fd910bcb67639c22df9f92470936cddc1ade0e2f2e7dc4f'
  },
  eosOptions: {
    chainId: 'cf057bbfb72640471fd910bcb67639c22df9f92470936cddc1ade0e2f2e7dc4f',
    debug: false,
    httpEndpoint: `http://localhost:8888`
  },
  endpoints: {
    api: 'http://localhost:4000/api',
    chat: 'http://chat.bespiral.local/chat',
    eosio: 'http://eosio.bespiral.local:8888',
    graphql: 'http://localhost:8080/v1alpha1/graphql',
    ipfs: 'http://ipfs.bespiral.local/ipfs'
  },
  logo: 'images/logo-cambiatus.svg',
  bespiralAccount: 'bespiral',
  communityContract: 'bes.cmm',
  tokenContract: 'bes.token',
  pushKey:
    'BDzXEdCCYafisu3jmYxBGDboAfwfIHYzM9BbT2DmL8VzIqSWu5BnW6lC-xEoXExQUS81vwOSPF9w8kpcINWCvUM'
}

const dev = {
  network: {
    blockchain: 'eos',
    protocol: 'http',
    host: 'eosio.bespiral.io',
    port: 80,
    chainId: 'cf057bbfb72640471fd910bcb67639c22df9f92470936cddc1ade0e2f2e7dc4f'
  },
  eosOptions: {
    chainId: 'cf057bbfb72640471fd910bcb67639c22df9f92470936cddc1ade0e2f2e7dc4f',
    debug: false,
    httpEndpoint: 'https://eosio.bespiral.io'
  },
  endpoints: {
    // api: 'http://localhost:4000',
    api: 'https://api.bespiral.io',
    // chat: 'http://app.bespiral.local/chat',
    chat: 'https://app.bespiral.io/chat',
    // eosio: 'http://eosio.bespiral.local:8888',
    eosio: 'https://eosio.bespiral.io',
    // graphql: 'http://localhost:4000/api/graph',
    graphql: 'https://api.bespiral.io/api/graph',
    // ipfs: 'http://ipfs.bespiral.local/ipfs',
    ipfs: 'https://ipfs.bespiral.io/ipfs'
  },
  logo: 'images/logo-cambiatus.svg',
  bespiralAccount: 'bespiral',
  communityContract: 'bes.cmm',
  tokenContract: 'bes.token',
  pushKey:
    'BDzXEdCCYafisu3jmYxBGDboAfwfIHYzM9BbT2DmL8VzIqSWu5BnW6lC-xEoXExQUS81vwOSPF9w8kpcINWCvUM'
}

const isLocal = typeof _env_ === 'undefined'

const chainUrl = isLocal ? dev.network.host : _env_.CHAIN_URL
const chainPort = isLocal ? dev.network.port : _env_.CHAIN_PORT
const chainId = isLocal ? dev.network.chainId : _env_.CHAIN_ID
const graphqlUrl = isLocal ? dev.endpoints.graphql : _env_.GRAPHQL_URL
const apiUrl = isLocal ? dev.endpoints.api : _env_.API_URL
const chatUrl = isLocal ? dev.endpoints.chat : _env_.CHAT_URL
const ipfsUrl = isLocal ? dev.endpoints.ipfs : _env_.IPFS_URL
const httpEndpoint = isLocal ? dev.eosOptions.httpEndpoint : `${chainUrl}:${chainPort}`
const pKey = isLocal ? dev.pushKey : _env_.PUSH_KEY
const appLogo = isLocal ? dev.logo : _env_.LOGO

const prod = {
  network: {
    blockchain: 'eos',
    protocol: 'https',
    host: chainUrl,
    port: chainPort,
    chainId: chainId
  },
  eosOptions: {
    chainId: chainId,
    debug: false,
    httpEndpoint: httpEndpoint
  },
  endpoints: {
    api: apiUrl,
    chat: chatUrl,
    eosio: httpEndpoint,
    graphql: graphqlUrl,
    ipfs: ipfsUrl
  },
  logo: appLogo,
  bespiralAccount: 'bespiral',
  communityContract: 'bes.cmm',
  tokenContract: 'bes.token',
  pushKey: pKey
}

export default {
  development: dev,
  local: local,
  production: prod
}

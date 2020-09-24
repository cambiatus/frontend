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
    socket: 'ws://localhost:4000/socket'
  },
  logo: '/images/logo-cambiatus.png',
  logoMobile: '/images/logo-cambiatus-mobile.svg',
  bespiralAccount: 'bespiral',
  communityContract: 'bes.cmm',
  tokenContract: 'bes.token',
  allowCommunityCreation: true,
  selectedCommunity: 'BES',
  pushKey:
    'BDzXEdCCYafisu3jmYxBGDboAfwfIHYzM9BbT2DmL8VzIqSWu5BnW6lC-xEoXExQUS81vwOSPF9w8kpcINWCvUM'
}

const dev = {
  network: {
    blockchain: 'eos',
    protocol: 'http',
    host: 'staging.cambiatus.io',
    port: 80,
    chainId: 'cf057bbfb72640471fd910bcb67639c22df9f92470936cddc1ade0e2f2e7dc4f'
  },
  eosOptions: {
    chainId: 'cf057bbfb72640471fd910bcb67639c22df9f92470936cddc1ade0e2f2e7dc4f',
    debug: false,
    httpEndpoint: 'https://staging.cambiatus.io'
  },
  endpoints: {
    api: 'https://staging.cambiatus.io',
    chat: 'https://app.cambiatus.io/chat',
    eosio: 'https://staging.cambiatus.io/',
    graphql: 'https://staging.cambiatus.io/api/graph',
    socket: 'wss://staging.cambiatus.io/api/socket'
    // api: 'https://api.cambiatus.io',
    // chat: 'https://app.cambiatus.io/chat',
    // eosio: 'https://eosio.cambiatus.io',
    // graphql: 'https://api.cambiatus.io/api/graph',
    // socket: 'wss://api.cambiatus.io/socket'
  },
  logo: '/images/logo-cambiatus.png',
  logoMobile: '/images/logo-cambiatus-mobile.svg',
  bespiralAccount: 'cambiatus',
  communityContract: 'cambiatus.cm',
  tokenContract: 'cambiatus.tk',
  allowCommunityCreation: true,
  selectedCommunity: 'CMB',
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
const httpEndpoint = isLocal
  ? dev.eosOptions.httpEndpoint
  : `${chainUrl}:${chainPort}`
const socketUrl = isLocal ? dev.endpoints.socket : _env_.SOCKET_URL
const pKey = isLocal ? dev.pushKey : _env_.PUSH_KEY
const appLogo = isLocal ? dev.logo : _env_.LOGO
const appLogoMobile = isLocal ? dev.logoMobile : _env_.LOGO_MOBILE
const allowCommunityCreation = isLocal
  ? dev.allowCommunityCreation
  : _env_.ALLOW_COMMUNITY_CREATION === 'true'
const selectedCommunity = isLocal
  ? dev.selectedCommunity
  : _env_.SELECTED_COMMUNITY
const communityContract = isLocal
  ? dev.communityContract
  : _env_.COMMUNITY_CONTRACT
const tokenContract = isLocal ? dev.tokenContract : _env_.TOKEN_CONTRACT

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
    socket: socketUrl
  },
  logo: appLogo,
  logoMobile: appLogoMobile,
  bespiralAccount: 'bespiral',
  communityContract: communityContract,
  tokenContract: tokenContract,
  allowCommunityCreation: allowCommunityCreation,
  selectedCommunity: selectedCommunity,
  pushKey: pKey
}

export default {
  development: dev,
  local: local,
  production: prod
}

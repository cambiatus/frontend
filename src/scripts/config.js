/* global _env_ */
const dev = {
  network: {
    blockchain: 'eos',
    protocol: 'http',
    host: 'staging.cambiatus.io',
    port: 80,
    chainId: 'fa087d6c692f16e01a9864749829359cd26b48db703377893f32ff1c72673a78'
  },
  eosOptions: {
    chainId: 'fa087d6c692f16e01a9864749829359cd26b48db703377893f32ff1c72673a78',
    debug: false,
    httpEndpoint: 'https://staging.cambiatus.io'
  },
  endpoints: {
    api: 'https://staging.cambiatus.io',
    chat: 'https://app.cambiatus.io/chat',
    eosio: 'https://staging.cambiatus.io/',
    graphql: 'https://staging.cambiatus.io/api/graph',
    socket: 'wss://staging.cambiatus.io/socket'
  },
  logo: '/images/logo-cambiatus.png',
  logoMobile: '/images/logo-cambiatus-mobile.svg',
  bespiralAccount: 'cambiatus',
  communityContract: 'cambiatus.cm',
  tokenContract: 'cambiatus.tk',
  allowCommunityCreation: true,
  useSubdomain: true,
  selectedCommunity: '0,CMB',
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
const useSubdomain = isLocal ? dev.useSubdomain : _env_.USE_SUBDOMAIN
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
  production: prod
}

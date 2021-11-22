import { Elm } from './elm/CambiatusBook.elm'
import './styles/main.css'
import './styles/elm-book.css'
import { register as registerCustomElements } from '../../src/customElements'
import configuration from '../../src/scripts/config'

const env = process.env.NODE_ENV || 'development'
const config = configuration[env]

const app = Elm.CambiatusBook.init()

const logBreadcrumb = (breadcrumb) => {
  const { message, ...rest } = breadcrumb
  console.log('[==== BREADCRUMB]: ', message, rest)
}

registerCustomElements(app, config, logBreadcrumb)

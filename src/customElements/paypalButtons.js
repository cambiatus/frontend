/* global HTMLElement, CustomEvent */

import * as paypalJs from '@paypal/paypal-js'
import usePortAsPromise from '../utils/usePortAsPromise'

export default (app, config) => (
  class PaypalButtons extends HTMLElement {
    static get observedAttributes () { return ['elm-currency'] }

    attributeChangedCallback (name, oldValue, newValue) {
      if (name === 'elm-currency') {
        this.removePaypal()
        this.setupPaypal()
      }
    }

    constructor () {
      super()

      const shadow = this.attachShadow({ mode: 'open' })
      this._paypalContainer = document.createElement('div')
      shadow.appendChild(this._paypalContainer)
    }

    disconnectedCallback () {
      this.removePaypal()
    }

    removePaypal () {
      for (const child of document.head.childNodes) {
        if (child.nodeName.toLowerCase() === 'script' && child.src) {
          if (child.src.startsWith('https://www.paypal.com/sdk/js?')) {
            document.head.removeChild(child)
          }
        }
      }
    }

    setupPaypal () {
      const handleError = (err) => {
        if (err.message !== 'Elm got an error creating contribution') {
          this.dispatchEvent(new CustomEvent('paypal-error', { error: err }))
        }
      }

      paypalJs.loadScript({
        'client-id': config.paypal.clientId,
        currency: this.getAttribute('elm-currency') || 'USD'
      })
        .then((paypal) => {
          paypal.Buttons({
            style: {
              shape: 'pill'
            },

            createOrder: async (data, actions) => {
              const valueAttribute = this.getAttribute('elm-value')
              if (valueAttribute === '') {
                return handleError(new Error('Amount could not be parsed as a float by Elm'))
              }

              app.ports.requestPaypalInfoFromJs.send(this.id)
              const paypalInfo = await usePortAsPromise(app.ports.paypalInfo, (paypalInfo) => {
                if (paypalInfo.targetId !== this.id) {
                  return { unsubscribeFromPort: false }
                }

                return paypalInfo
              })

              if (paypalInfo.error) {
                return handleError(new Error('Elm got an error creating contribution'))
              }

              return actions.order.create({
                purchase_units: [{
                  invoice_id: paypalInfo.invoiceId,
                  amount: {
                    value: paypalInfo.amount,
                    currency_code: paypalInfo.currency
                  }
                }],
                application_context: {
                  brand_name: paypalInfo.communityName,
                  shipping_preference: 'NO_SHIPPING'
                }
              })
            },

            onApprove: (data, actions) => {
              return actions.order.capture().then(() => {
                this.dispatchEvent(new CustomEvent('paypal-approve', {}))
              })
            },

            onCancel: () => {
              this.dispatchEvent(new CustomEvent('paypal-cancel', {}))
            },

            onError: handleError
          }).render(this._paypalContainer)
        })
        .catch((err) => {
          this.dispatchEvent(new CustomEvent('paypal-load-error', { error: err }))
        })
    }
  }
)

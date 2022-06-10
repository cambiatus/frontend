import bgNoScroll from './bgNoScroll'
import focusTrap from './focusTrap'
import infiniteList from './infiniteList'
import keyListener from './keyListener'
import maskedInputHelper from './maskedInputHelper'
import paypalButtons from './paypalButtons'
import richTextEditor from './richTextEditor'
import dateFormatter from './dateFormatter'
import dialogBubble from './dialogBubble'
import pdfViewer from './pdfViewer'
import intersectionObserverElement from './intersectionObserverElement'
import masonryLayout from './masonryLayout'
import pointerListener from './pointerListener'
import imageCropper from './imageCropper'

const customElements = {
  'bg-no-scroll': bgNoScroll,
  'date-formatter': dateFormatter,
  'dialog-bubble': dialogBubble,
  'focus-trap': focusTrap,
  'infinite-list': infiniteList,
  'key-listener': keyListener,
  'masked-input-helper': maskedInputHelper,
  'paypal-buttons': paypalButtons,
  'pdf-viewer': pdfViewer,
  'richtext-editor': richTextEditor,
  'intersection-observer': intersectionObserverElement,
  'masonry-layout': masonryLayout,
  'pointer-listener': pointerListener,
  'image-cropper': imageCropper
}

const register = (app, config, addBreadcrumb) => {
  Object.keys(customElements).forEach((key) => {
    window.customElements.define(key, customElements[key](app, config, addBreadcrumb))
  })
}

/**
 * Each element in the object is a pair of custom element name and function that
 * builds the class that represents the custom element. The function takes in
 * the Elm app, followed by the `config` object, and the function to `addBreadcrumb`s
 */
export { customElements, register }

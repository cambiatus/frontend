const _ = require('lodash')

module.exports = {
  // There are a two different ways of changing tailwind behaviour:
  // 1) Overwriting the default behaviour (this is what 'fontWeight' is doing)
  // 2) Add a new behaviour to the current one (using the 'extend' key below)
  theme: {
    // Customize checkbox Icon for forms
    customForms: theme => ({
      default: {
        checkbox: {
          icon: '<svg width="12" height="12" viewBox="0 0 12 12" fill="none" xmlns="http://www.w3.org/2000/svg"><rect width="12" height="12" rx="2" fill="#8ACC9E"/></svg>'
        }
      }
    }),
    // Colors used on the 'Design System'
    colors: {
      'black': '#000000',
      'white': '#FFFFFF',
      gray: {
        // '100': '#f5f5f5', // Tailwind original gray-100
        '100': '#F8F8F8',
        '200': '#eeeeee',
        '300': '#e0e0e0',
        '400': '#bdbdbd',
        '500': '#E7E7E7',
        // '500': '#9e9e9e', // Tailwind original gray-500
        // '600': '#757575',
        '600': '#9e9e9e', // Tailwind original gray-500
        '700': '#616161',
        '800': '#424242',
        // '900': '#212121' // Tailwind original gray-900
        '900': '#999999'
      },
      purple: {
        '100': '#9B4198',
        '500': '#45469B'
      },
      'red': '#DB1B1B',
      'yellow': '#FFD200',
      'green': '#8ACC9E',
      'blue': '#00BDCD',
      'orange': {
        '100': '#FAB15C',
        '300': '#F99D33',
        '500': '#F2663B'
      },
      'indigo': {
        '100': '#6A6CAA',
        '500': '#45469B'
      }
    },
    // Customizing BorderRadius sizes
    borderRadius: {
      'none': '0',
      'sm': '.125rem',
      default: '.4rem',
      'lg': '1.25rem',
      'full': '9999px'
    },
    // Overwriting since the current font family only have these weights
    fontWeight: {
      'light': 300,
      'normal': 400,
      'medium': 500,
      'bold': 700
    },
    // transform keys
    rotate: {
      '-45': '-45deg',
      '135': '135deg',
      '180': '180deg'
    },
    extend: {
      // TODO: Below are the colors that need to be removed after refactor
      colors: {
        'body-blue': '#45469B',
        'grey': '#D4D4D4',
        'light-grey': '#EEEEEE',
        'text-grey': '#666666',
        'border-grey': '#E0E0E0',
        'reward-green': '#8ACC9E',
        'date-purple': '#45469B',
        'button-orange': '#F99D33',
        'date-red': '#E02020'
      },
      flexGrow: {
        '1': 1,
        '2': 2,
        '3': 3,
        '4': 4,
        '5': 5,
        '6': 6,
        '7': 7,
        '8': 8,
        '9': 9
      },
      fontFamily: {
        'sans': ['"Gotham Rounded"', 'sans-serif']
      },
      // TODO: move this to parent section to overwrite current behaviour
      fontSize: {
        'caption': '.625rem',
        'menu': '.8125rem',
        'body': '.875rem',
        'heading': '1.375rem',
        'title-h3': '1.875rem',
        'title-h2': '2.375rem',
        'title-h1': '3rem',
        'hero': '3.375rem'
      },
      spacing: {
        '7': '1.75rem',
        'heading': '1.375rem',
        '14': '3.5rem',
        'select': '18.75rem'
      },
      lineHeight: {
        caption: 0.75
      },
      borderRadius: {
        'sm': '0.25rem',
        default: '0.75rem',
        'super': '2.5rem'
      },
      inset: {
        '1': '1rem',
        '1/2': '50%'
      }
    }
  },
  variants: {
    // If you're going to override one of:
    // - backgroundColor
    // - borderColor
    // - boxShadow
    // - fontWeight
    // - opacity
    // - outline
    // - textColor
    // - textDecoration
    // Make sure to also include 'hover' and 'focus' (or check if none is used in the project)
    // Not including them may break the current behaviour, because 'variants' key overwrites the current tailwind behaviour
    // See an example below of how add a varian to 'last':
    // backgroundColor: ['hover', 'focus', 'last']
    borderRadius: ['responsive', 'first-hover', 'last-hover', 'last', 'first'],
    borderWidth: ['last']
  },
  plugins: [
    require('tailwindcss-transforms')({
      '3d': false // defaults to false
    }),
    require('@tailwindcss/custom-forms'),
    // Pseudo-class to support hover on the parent's first child
    function ({ addVariant, e }) {
      addVariant('first-hover', ({ modifySelectors, separator }) => {
        modifySelectors(({ className }) => {
          return `.${e(`first-hover${separator}${className}`)}:first-child:hover`
        })
      })
    },
    // Pseudo-class to support hover on the parent's last child
    function ({ addVariant, e }) {
      addVariant('last-hover', ({ modifySelectors, separator }) => {
        modifySelectors(({ className }) => {
          return `.${e(`last-hover${separator}${className}`)}:last-child:hover`
        })
      })
    },
    // class support for rotation
    function ({ addUtilities, config }) {
      const rotateUtilities = _.map(config('theme.rotate'), (value, key) => {
        return {
          [`.${`rotate-${key}`}`]: {
            transform: `rotate(${value})`,
            '-webkit-transform': `rotate(${value})`
          }
        }
      })

      addUtilities(rotateUtilities)
    }
  ]
}

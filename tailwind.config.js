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
          width: theme('spacing.6'),
          height: theme('spacing.6'),
          icon: '<svg width="22" height="22" viewBox="0 0 22 22" fill="none" xmlns="http://www.w3.org/2000/svg">' +
            '<rect x="0.5" y="0.5" width="21" height="21" rx="2.5" fill="white" stroke="#E7E7E7"/>' +
            '<rect x="5" y="5" width="12" height="12" rx="2" fill="#8ACC9E"/>' +
            '</svg>'
        },
        radio: {
          width: theme('spacing.6'),
          height: theme('spacing.6'),
          icon: '<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">' +
            '<circle cx="12" cy="12" r="11.5" fill="white" stroke="#E7E7E7"/>' +
            '<path d="M18 12C18 15.3137 15.3137 18 12 18C8.68629 18 6 15.3137 6 12C6 8.68629 8.68629  6 12 6C15.3137 6 18 8.68629 18 12Z" fill="#8ACC9E"/>' +
            '</svg>',
          '&:disabled': {
            icon: '<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">' +
            '<circle cx="12" cy="12" r="11.5" fill="white" stroke="#E7E7E7"/>' +
            '<path d="M18 12C18 15.3137 15.3137 18 12 18C8.68629 18 6 15.3137 6 12C6 8.68629 8.68629  6 12 6C15.3137 6 18 8.68629 18 12Z" fill="#999999"/>' +
            '</svg>'
          }
        }
      }
    }),
    // Colors used on the 'Design System'
    colors: {
      black: '#000000',
      white: '#FFFFFF',
      gray: {
        '100': '#F8F8F8',
        '200': '#eeeeee',
        '300': '#e0e0e0',
        '400': '#bdbdbd',
        '500': '#E7E7E7',
        '700': '#616161',
        '800': '#424242',
        '900': '#999999'
      },
      purple: {
        '100': '#9B4198',
        '500': '#45469B'
      },
      red: '#DB1B1B',
      lightred: '#F56565',
      yellow: '#FFD200',
      green: '#8ACC9E',
      blue: '#00BDCD',
      orange: {
        '100': '#FAB15C',
        '300': '#F99D33',
        '500': '#F2663B'
      },
      indigo: {
        '100': '#6A6CAA',
        '500': '#45469B'
      }
    },
    // Needed after updating to tailwind 2.0
    screens: {
      'xs-max': {
        'max': '320px'
      },
      sm: '640px',
      md: '768px',
      lg: '1024px',
      xl: '1280px'
    },
    // Customizing BorderRadius sizes
    borderRadius: {
      'none': '0',
      'sm': '0.125rem',
      DEFAULT: '.4rem',
      default: '.4rem',
      label: '.313rem', // 5px
      'md': '0.375rem',
      'lg': '1.25rem',
      'full': '9999px',
      'large': '12px'
    },
    // Overwriting since the current font family only have these weights
    fontWeight: {
      light: 300,
      normal: 400,
      medium: 600,
      bold: 700
    },
    // transform keys
    rotate: {
      '-90': '-90deg',
      '-45': '-45deg',
      '90': '90deg',
      '135': '135deg',
      '180': '180deg'
    },
    extend: {
      colors: {
        'body-blue': '#45469B',
        grey: '#D4D4D4',
        'light-grey': '#EEEEEE',
        'text-grey': '#666666',
        'border-grey': '#E0E0E0',
        'body-black': '#4a4a4a',
        'phone': '#999999',
        'instagram': '#e1306c',
        'telegram': '#30a8db',
        'whatsapp': '#25d366'
      },
      boxShadow: {
        outline: '0 0 0 3px rgb(250, 177, 92, 0.75)'
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
        sans: ['Nunito', 'sans-serif']
      },
      fontSize: {
        caption: '.625rem',
        menu: '.9125rem',
        body: '.9rem',
        heading: '1.375rem',
        'title-h3': '1.875rem',
        'title-h2': '2.375rem',
        'title-h1': '3rem',
        hero: '3.375rem'
      },
      spacing: {
        '7': '1.75rem',
        heading: '1.375rem',
        '14': '3.5rem',
        '29': '7.25rem',
        '44': '11rem',
        '72': '18rem',
        '80': '20rem',
        select: '18.75rem',
        '1/3': '33%',
        '1/2': '50%'
      },
      maxHeight: {
        '108': '27rem'
      },
      lineHeight: {
        caption: 0.75
      },
      borderRadius: {
        xs: '0.125rem',
        sm: '0.25rem',
        default: '0.75rem',
        super: '2.5rem'
      },
      inset: {
        '1': '1rem',
        '1/2': '50%',
        modal: '10rem'
      },
      width: {
        form: '45.625rem'
      },
      opacity: {
        '10': '0.1',
        '60': '0.6'
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
    require('@tailwindcss/forms'),
    // Pseudo-class to support hover on the parent's first child
    function ({ addVariant, e }) {
      addVariant('first-hover', ({ modifySelectors, separator }) => {
        modifySelectors(({ className }) => {
          return `.${e(
            `first-hover${separator}${className}`
          )}:first-child:hover`
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
  ],
  purge: [
    './src/**/*.elm',
    './src/index.js'
  ]
}

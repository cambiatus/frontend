const _ = require('lodash')

module.exports = {
  // There are a two different ways of changing tailwind behaviour:
  // 1) Overwriting the default behaviour (this is what 'fontWeight' is doing)
  // 2) Add a new behaviour to the current one (using the 'extend' key below)
  theme: {
    // Colors used on the 'Design System'
    colors: {
      transparent: 'transparent',
      black: '#000000',
      white: '#FFFFFF',
      gray: {
        '100': '#F8F8F8',
        '200': '#eeeeee',
        '300': '#e0e0e0',
        '333': '#333333',
        '400': '#bdbdbd',
        '500': '#E7E7E7',
        '600': '#9E9E9E',
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
      blue: {
        DEFAULT: '#00BDCD',
        '600': '#4299E1'
      },
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
    // The first item in each list is the font size, and the second item is the
    // default line height for that font size
    fontSize: {
      xs: ['0.6875rem', '0.75rem'], // ['11px', '12px']
      sm: ['0.75rem', '0.9375rem'], // ['12px', '15px']
      base: ['0.9375rem', '1.375rem'], // ['15px', '22px']
      lg: ['1.375rem', '1.875rem'], // ['22px', '30px']
      xl: ['1.875rem', '2.5625rem'], // ['30px', '41px']
      '2xl': ['2.375rem', '3.25rem'], // ['38px', '52px']
      '3xl': ['2.875rem', '3.9375rem'], // ['46px', '63px']
      '4xl': ['3.375rem', '4.625rem'] // ['54px', '74px']
    },
    // Customizing BorderRadius sizes
    borderRadius: {
      'none': '0',
      DEFAULT: '0.4rem',
      label: '0.313rem',
      'md': '0.375rem',
      'lg': '1.25rem',
      'full': '9999px',
      'large': '12px'
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
      spacing: {
        '3px': '3px',
        heading: '1.375rem',
        '7': '1.75rem',
        '9': '2.25rem',
        '14': '3.5rem',
        '18': '4.5rem',
        '29': '7.25rem',
        '44': '11rem',
        '72': '18rem',
        '80': '20rem',
        select: '18.75rem',
        '1/3': '33%',
        '1/2': '50%'
      },
      zIndex: {
        '-10': '-10'
      },
      maxHeight: {
        '108': '27rem'
      },
      minHeight: {
        '25': '6.25rem',
        '36': '9rem',
        '48': '12rem'
      },
      lineHeight: {
        caption: 0.75
      },
      borderRadius: {
        xs: '0.125rem',
        sm: '0.25rem',
        super: '2.5rem'
      },
      inset: {
        '1': '1rem',
        '-1': '-0.25rem',
        '1/2': '50%',
        modal: '10rem',
        'full': '100%'
      },
      width: {
        form: '45.625rem',
        '120': '30rem'
      },
      minWidth: {
        '30': '7.5rem'
      },
      opacity: {
        '10': '0.1',
        '60': '0.6'
      },
      keyframes: {
        appear: {
          '0%': {
            opacity: '0',
            'pointer-events': 'none'
          },
          '100%': {
            opacity: '1',
            'pointer-events': 'auto'
          }
        },
        'appear-from-above': {
          '0%': { opacity: 0, transform: 'translate(0, -5px)' },
          '100%': { opacity: 1, transform: 'translate(0, 0)' }
        },
        'appear-from-above-lg': {
          '0%': { opacity: 0, transform: 'translate(0, -100px)' },
          '50%': { opacity: 0 },
          '100%': { opacity: 1, transform: 'translate(0, 0)' }
        },
        'skeleton-loading-keyframes': {
          '0%': {
            'background-color': '#e0e0e0'
          },
          '100%': {
            'background-color': '#eeeeee'
          }
        }
      },
      animation: {
        'fade-in': 'appear 50ms linear 400ms both',
        'fade-in-from-above': 'appear-from-above 75ms ease-out',
        'fade-in-from-above-lg': 'appear-from-above-lg 600ms ease-in-out both',
        'skeleton-loading': 'skeleton-loading-keyframes 1s ease-out infinite alternate'
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
  ],
  mode: 'jit'
}

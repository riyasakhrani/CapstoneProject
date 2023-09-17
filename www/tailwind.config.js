/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ['./*{html,js}'],
  theme: {colors: {
    'body': '#F2F2F2',
    'selected-text': '#3876F2',
    gray: {
      1: '#2E343F',
      2: '#4F5357'
    },
    white: {
      1: '#FFFFFF',
      2: '#F2F2F2'
    },
     blue: {
      1: '#3854F2',
      2: '#3876F2',
      3: '#36ABD9',
      4: '#30BFBF',
      5: '#32D9BA'
    } 
  },
  
  fontFamily: {
      'poppins': ["'Poppins'", 'sans-serif']
    },
    
    extend: {},
  },
  plugins: [],
}


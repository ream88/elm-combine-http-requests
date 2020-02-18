const path = require('path')
const HtmlWebpackPlugin = require('html-webpack-plugin')

module.exports = {
  module: {
    rules: [{
      test: /\.elm$/,
      exclude: [/elm-stuff/, /node_modules/],
      use: {
        loader: 'elm-webpack-loader',
        options: {}
      }
    }]
  },
  devServer: {
    contentBase: path.join('public')
  },
  plugins: [
    new HtmlWebpackPlugin()
  ]
}

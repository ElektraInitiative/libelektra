var webpack = require('webpack')
var path = require('path')

module.exports = {
  context: __dirname,
  entry: [
    'webpack/hot/only-dev-server', // "only" prevents reload on syntax errors
    path.join(__dirname, '/src/index.js'),
  ],
  output: { path: path.join(__dirname, '/dist'), filename: 'elektra-web.js' },
  plugins: [
    new webpack.HotModuleReplacementPlugin(),
    new webpack.DefinePlugin({
      'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV || 'development'),
    }),
  ],
  module: {
    loaders: [
      {
        test: /.jsx?$/,
        exclude: /node_modules/,
        loader: 'babel-loader',
        query: {
          presets: ['es2015', 'react'],
        },
      },
    ],
  },
}

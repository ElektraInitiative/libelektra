var webpack = require('webpack')
var path = require('path')

const entry = [
  path.join(__dirname, '/src/index.js'),
]

const plugins = [
  new webpack.DefinePlugin({
    'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV || 'development'),
  }),
]

if (process.env.NODE_ENV !== 'production') {
  entry.push('webpack/hot/only-dev-server') // "only" prevents reload on syntax errors
  plugins.push(new webpack.HotModuleReplacementPlugin())
}

module.exports = {
  context: __dirname,
  entry: entry,
  output: { path: path.join(__dirname, '/dist'), filename: 'elektra-web.js' },
  plugins: plugins,
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

var webpack = require('webpack')
var DashboardPlugin = require('webpack-dashboard/plugin')

module.exports = {
  entry: [
    'webpack-dev-server/client?http://0.0.0.0:8080', // WebpackDevServer host and port
    'webpack/hot/only-dev-server', // "only" prevents reload on syntax errors
    './src/index.js',
  ],
  output: { path: __dirname, filename: 'elektra-web.js' },
  plugins: [
    new DashboardPlugin(),
    new webpack.HotModuleReplacementPlugin(),
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

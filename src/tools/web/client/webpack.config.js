var DashboardPlugin = require('webpack-dashboard/plugin')

module.exports = {
  entry: './src/index.js',
  output: { path: __dirname, filename: 'elektra-web.js' },
  plugins: [
    new DashboardPlugin(),
  ],
  module: {
    loaders: [
      {
        test: /.jsx?$/,
        loader: 'babel-loader',
        exclude: /node_modules/,
        query: {
          presets: ['es2015', 'react'],
        },
      },
    ],
  },
}

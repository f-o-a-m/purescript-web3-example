const appConfig = require('./src/App/Config.js').config;
const path = require('path');
const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');

const isProd = process.env.NODE_ENV === 'production';

const entries = [path.join(__dirname, 'support/entry.js')];

const ConsoleNotifierPlugin = function () {};

ConsoleNotifierPlugin.prototype.compilationDone = (stats) => {
    const log = (error) => {
         console.log(error);
    };
    stats.compilation.errors.forEach(log);
};

ConsoleNotifierPlugin.prototype.apply = function (compiler) {
    compiler.plugin('done', this.compilationDone.bind(this));
};

const plugins = [
  new webpack.DefinePlugin({
    'process.env.SIMPLE_STORAGE_ADDRESS': JSON.stringify(process.env.SIMPLE_STORAGE_ADDRESS)
  }),
  new ConsoleNotifierPlugin(),
  new HtmlWebpackPlugin({
    template: 'index.html'
  })
];

if (isProd) {
  plugins.push(
    new webpack.LoaderOptionsPlugin({
      minimize: true,
      debug: false
    })
  );
}

module.exports = {
  entry: entries,
  target: 'web',
  output: {
    filename: 'app.js',
    path: __dirname + '/dist',
    publicPath: ''
  },
  devtool: "source-map",
  module: {
  loaders: [
      {
          test: /\.js$/,
          exclude: /node_modules/,
          loader: 'babel-loader',
          options: {
          }
      },
      {
        test: /\.purs$/,
        loader: 'purs-loader',
        exclude: /node_modules/,
        query: {
          psc: 'psa',
          src: ['bower_components/*/src/**/*.purs', 'src/**/*.purs'],
          pscIde: true,
          pscPackage: false
        }
      },
      {
        test: /\.scss$/,
        use: [{
          loader: "style-loader" // creates style nodes from JS strings
        }, {
          loader: "css-loader" // translates CSS into CommonJS
        }, {
          loader: "sass-loader" // compiles Sass to CSS
        }]
      }
    ]
  },
  plugins: plugins,
  resolveLoader: {
    modules: [
      path.join(__dirname, 'node_modules')
    ]
  },
  resolve: {
    modules: [
      'node_modules',
      'bower_components'
    ],
    extensions: ['.js', '.purs']
  },
  performance: { hints: false },
  stats: {
    hash: false,
    timings: false,
    version: false,
    assets: false,
    errors: true,
    colors: false,
    chunks: false,
    children: false,
    cached: false,
    modules: false,
    chunkModules: false
  },
  devServer: {
      port: 8080,
      host: "0.0.0.0",
      disableHostCheck: true,
  }
}

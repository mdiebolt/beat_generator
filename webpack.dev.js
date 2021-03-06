var path = require("path");
var webpack = require("webpack");
var merge = require("webpack-merge");
var HtmlWebpackPlugin = require("html-webpack-plugin");
var autoprefixer = require("autoprefixer");

// entry and output path/filename variables
const entryPath = path.join(__dirname, "src/static/index.js");
const outputPath = path.join(__dirname, "dist");
const outputFilename = "[name].js";

var commonConfig = {
  output: {
    path: outputPath,
    filename: `static/js/${outputFilename}`
  },
  resolve: {
    extensions: [".js", ".elm"],
    modules: ["node_modules"]
  },
  module: {
    noParse: /\.elm$/,
    rules: [
      {
        test: /\.(eot|ttf|woff|woff2|svg)$/,
        use: "file-loader?publicPath=../../&name=static/css/[hash].[ext]"
      }
    ]
  },
  plugins: [
    new webpack.LoaderOptionsPlugin({
      options: {
        postcss: [autoprefixer()]
      }
    }),
    new HtmlWebpackPlugin({
      template: "src/static/index.html",
      inject: "body",
      filename: "index.html"
    })
  ]
};

module.exports = merge(commonConfig, {
  entry: ["webpack-dev-server/client?http://localhost:8080", entryPath],
  devServer: {
    // serve index.html in place of 404 responses
    historyApiFallback: true,
    contentBase: "./src",
    hot: true
  },
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          {
            loader: "elm-webpack-loader",
            options: {
              verbose: true,
              warn: true,
              debug: true
            }
          }
        ]
      },
      {
        test: /\.sc?ss$/,
        use: ["style-loader", "css-loader", "postcss-loader", "sass-loader"]
      }
    ]
  }
});

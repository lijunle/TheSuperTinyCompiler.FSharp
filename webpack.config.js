var path = require("path");
var webpack = require("webpack");

module.exports = {
  devtool: "source-map",
  entry: "./TheSuperTinyCompiler.FSharp/dist/Program",
  output: {
    path: path.join(__dirname, "TheSuperTinyCompiler.FSharp/bin"),
    filename: "bundle.js"
  },
  module: {
    preLoaders: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        loader: "source-map-loader"
      }
    ]
  }
};

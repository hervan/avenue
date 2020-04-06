const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const { CleanWebpackPlugin } = require("clean-webpack-plugin");

module.exports = {
  entry: ["./src/Index.bs.js"],
  mode: "production",
  plugins: [
    new CleanWebpackPlugin(),
    new HtmlWebpackPlugin({
      title: "avenue",
    }),
  ],
  output: {
    path: path.join(__dirname, "bundleOutput"),
    filename: "avenue.[contenthash].js",
  },
};

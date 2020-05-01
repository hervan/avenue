const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const { CleanWebpackPlugin } = require("clean-webpack-plugin");
const WebpackPwaManifest = require("webpack-pwa-manifest");
const FaviconsWebpackPlugin = require("favicons-webpack-plugin");

module.exports = {
  entry: ["./src/Index.bs.js"],
  mode: "production",
  plugins: [
    new CleanWebpackPlugin(),
    new HtmlWebpackPlugin({
      title: "avenue",
      meta: {
        viewport:
          "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no, shrink-to-fit=no",
      },
    }),
    new WebpackPwaManifest({
      name: "Avenue",
      short_name: "avenue",
      description: "SVG implementation of flip and write game Avenue",
      background_color: "lightgreen",
      orientation: "landscape",
      display: "fullscreen",
      ios: "startup",
    }),
    new FaviconsWebpackPlugin("./src/assets/icon.svg"),
  ],
  output: {
    path: path.join(__dirname, "bundleOutput"),
    filename: "avenue.[contenthash].js",
  },
};

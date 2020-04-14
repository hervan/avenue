const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const { CleanWebpackPlugin } = require("clean-webpack-plugin");
const WebpackPwaManifest = require("webpack-pwa-manifest");

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
      crossorigin: "use-credentials", //can be null, use-credentials or anonymous
      icons: [
        {
          src: path.resolve("src/assets/icon.svg"),
          sizes: [96, 128, 192, 256, 384, 512, 1024],
        },
      ],
    }),
  ],
  output: {
    path: path.join(__dirname, "bundleOutput"),
    filename: "avenue.[contenthash].js",
  },
};

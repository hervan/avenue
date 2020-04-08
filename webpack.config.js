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
    }),
    new WebpackPwaManifest({
      name: "Avenue",
      short_name: "avenue",
      description: "SVG implementation of flip and write game Avenue",
      background_color: "lightgreen",
      crossorigin: "use-credentials", //can be null, use-credentials or anonymous
      // icons: [
      //   {
      //     src: path.resolve("src/assets/icon.png"),
      //     sizes: [96, 128, 192, 256, 384, 512], // multiple sizes
      //   },
      //   {
      //     src: path.resolve("src/assets/large-icon.png"),
      //     size: "1024x1024", // you can also use the specifications pattern
      //   },
      //   {
      //     src: path.resolve("src/assets/maskable-icon.png"),
      //     size: "1024x1024",
      //     purpose: "maskable",
      //   },
      // ],
    }),
  ],
  output: {
    path: path.join(__dirname, "bundleOutput"),
    filename: "avenue.[contenthash].js",
  },
};

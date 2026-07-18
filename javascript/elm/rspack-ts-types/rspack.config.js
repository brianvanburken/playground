const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");

module.exports = (env, argv) => {
  const isDev = argv.mode !== "production";

  return {
    entry: "./src/index.js",
    output: {
      path: path.resolve(__dirname, "dist"),
      filename: "[name].js",
    },
    module: {
      rules: [
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: {
            loader: "elm-webpack-loader",
            options: {
              debug: isDev,
              optimize: !isDev,
            },
          },
        },
      ],
    },
    plugins: [
      new HtmlWebpackPlugin({
        template: "./src/index.html",
      }),
    ],
    devServer: {
      hot: true,
      static: path.resolve(__dirname, "dist"),
    },
  };
};

const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const elmTypesPlugin = require("./scripts/ElmTypesPlugin");

module.exports = (env, argv) => {
  const isDev = argv.mode !== "production";

  return {
    entry: "./src/index.ts",
    output: {
      path: path.resolve(__dirname, "dist"),
      filename: "[name].js",
    },
    resolve: {
      extensions: [".ts", ".js"],
    },
    module: {
      rules: [
        {
          test: /\.ts$/,
          exclude: [/node_modules/],
          loader: "builtin:swc-loader",
          options: {
            jsc: {
              parser: {
                syntax: "typescript",
              },
            },
          },
        },
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
      elmTypesPlugin(),
    ],
    devServer: {
      hot: true,
      static: path.resolve(__dirname, "dist"),
    },
  };
};

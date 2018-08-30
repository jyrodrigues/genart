var path = require("path");

module.exports = {
  entry: {
    app: [
      "./src/index.js"
    ]
  },

  output: {
    path: path.resolve(__dirname + "/dist"),
    filename: "[name].js",
  },

  module: {
    rules: [
      {
        test: /\.s?css$/,
        use: [
          "style-loader",
          "css-loader",
        ]
      },
      {
        test: /\.html$/,
        exclude: /node_modules/,
        loader: "file-loader?name=[name].[ext]",
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: "elm-webpack-loader?verbose=true",
      },
      {
        test: /\.svg$/,
        loader: "file-loader"
      },
    ],

    noParse: /\.elm$/,
  },

  devServer: {
    inline: true,
    stats: { colors: true },
  },


}
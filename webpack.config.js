var path = require('path');
var HtmlwebpackPlugin = require('html-webpack-plugin');
var webpack = require('webpack');
var merge = require('webpack-merge');


process.env.BABEL_ENV = process.env.npm_lifecycle_event;
var TARGET = process.env.npm_lifecycle_event;
var ROOT_PATH = path.resolve(__dirname);
var APP_PATH = path.resolve(ROOT_PATH, 'app');
var BUILD_PATH = path.resolve(ROOT_PATH, 'build');

var common = {
    entry: APP_PATH,
    resolve:{
        extensions: ['', '.js', '.jsx', '.json', '.scss'],
    },
    output: {
        path: BUILD_PATH,
        filename: 'bundle.js'
    },
    module: {
        loaders: [
        {
            test: /(\.css|\.scss)$/,
            loader: 'style!css?modules&importLoaders=1&localIdentName=[name]__[local]___[hash:base64:5]!postcss!sass'
        },
        {
            test: /\.jsx?$/,
            loaders: ['babel'],
            include: APP_PATH
        }
        ]
    },
    plugins: [
        new HtmlwebpackPlugin({
            title: 'ChatQY',
            template: 'assets/index.html',
            inject: 'body'
        })
        ]
}

if (TARGET === 'start' || !TARGET) {
    module.exports = merge(common, {
        devtool: 'eval-source-map',
        devServer: {
            historyApiFallback: true,
            hot: true,
            inline: true,
            progress: true
        },
        plugins: [
            new webpack.HotModuleReplacementPlugin()
            ]
    });
}

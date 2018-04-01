# purescript-web3-example

[![Build Status](https://travis-ci.org/f-o-a-m/purescript-web3-example.svg?branch=master)](https://travis-ci.org/f-o-a-m/purescript-web3-example)

## Purescript Web3 Example
This is an example project using [purescript-web3](https://github.com/f-o-a-m/purescript-web3) and [purescript-web3-generator](https://github.com/f-o-a-m/purescript-web3-generator) to interact with a very simple smart
contract -- just storing, updating, and querying a 256-bit unsigned integer. It uses [Thermite](https://github.com/paf31/purescript-thermite)
and [purescript-react](https://github.com/purescript-contrib/purescript-react) for the ui.

It should be easy to clone this repo as a template for other web3 enabled applications. It has a [Webpack configuration](./webpack.config.js) for introducing contract addresses as environment
variables to your application, as well as all of the loaders one would need for a real world PureScript application.

## Requirements

Note: In addition to the following requirements, because the example app now depends on being able to use [uPort](https://www.uport.me/) if the user desires, *you must point your MetaMask account to a Rinkeby node*

- [MetaMask](https://chrome.google.com/webstore/detail/metamask/nkbihfbeogaeaoehlefnkodbefgpgknn) Chrome extension
- an Ethereum address (with a nonzero Ether balance) known to MetaMask on [`Rinkeby` network](https://www.rinkeby.io)
- an address for an uploaded [`SimpleStorage`](./contracts/src/SimpleStorage.sol) contract.


## Run Instructions

Using `npm`
```bash
> npm install
> npm run generator
> env SIMPLE_STORAGE_ADDRESS=<your-contract-address> npm run webpack-dev-server
```

or by using `Yarn`
```shell
> yarn
> yarn generator
> env SIMPLE_STORAGE_ADDRESS=<your-contract-address> yarn webpack-dev-server
```

# purescript-web3-example

[![Build Status](https://travis-ci.org/f-o-a-m/purescript-web3-example.svg?branch=travis)](https://travis-ci.org/f-o-a-m/purescript-web3-example)

## Purescript Web3 Example
This is an example project using [purescript-web3](https://github.com/f-o-a-m/purescript-web3) and [purescript-web3-generator](https://github.com/f-o-a-m/purescript-web3-generator) to interact with a very simple smart
contract -- just storing, updating, and querying a 256-bit unsigned integer. It uses [thermite](https://github.com/paf31/purescript-thermite)
and [purescript-react](https://github.com/purescript-contrib/purescript-react) for the ui.

It should be easy to clone this repo as a template for other web3 enabled applications. It has a webpack configuration for introducing contract addresses as environment
variables to your application, as well as all of the loaders one would need for a real world purescript application.

## Requirements
- [metamask](https://chrome.google.com/webstore/detail/metamask/nkbihfbeogaeaoehlefnkodbefgpgknn) chrome extension
- an ethereum address (with a nonzero ether balance) known to metamask on some network
- an address for an uploaded SimpleStorage contract (see ./contracts/src/SimpleStorage.sol)
- psc-package


## Run Instructions
```bash
> npm install
> env SIMPLE_STORAGE_ADDRESS=<your-contract-address> npm run webpack-dev-server
```

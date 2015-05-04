# giflib web [![Build Status](https://travis-ci.org/passy/giflib-web.svg?branch=master)](https://travis-ci.org/passy/giflib-web)

Not sure if I'll ever finish this. Right now just a playground for
PureScript and Halogen.

## Setup

```bash
$ bower install
$ npm install
$ npm run build
```

## Development

```bash
# Continously build in the background
$ pulp -w build

# Run and watch the tests
$ pulp -w test

# Build for "production"
$ gulp

# Run a dev server
$ npm run serve

# Open the web page
$ open http://localhost:8080
```

Using `pulp build` is a lot faster than gulp, but unfortunately you'll have to
go through the long `gulp` process if you actually want to see the page in the
browser. Luckily, we're using PureScript here so you don't need your browser
most of the time during development. Type safety and stuff.

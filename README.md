# Haskell Web Starter

A simple template for getting started on Haskell web development. This provides a bare-bones reading tracking app to show how you can serve web pages and REST APIs in Haskell.

This can be used to get started with JSON API webservices, client-facing HTML web apps, or both.

## Features

- http routing with [scotty](https://hackage.haskell.org/package/scotty)
- HTML5 rendering/templates with [blaze-html](https://hackage.haskell.org/package/blaze-html)
- Static file serving
- JSON REST API using [aeson](https://hackage.haskell.org/package/aeson)
- CORS enabled
- Postgres database connection using [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple)
- App building and serving with [Cabal](https://hackage.haskell.org/package/Cabal)
- VSCode integration/intellisense with the [Haskell Extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)

## Requires

- [Cabal](https://hackage.haskell.org/package/Cabal)
- [Postgres](https://www.postgresql.org/) instance

## Running

Edit the Postgres connection info in `app/Main.hs`

```hs
localPG :: ConnectInfo
localPG =
  defaultConnectInfo
    { connectDatabase = "postgres",
      connectHost = "localhost",
      connectPassword = "1234"
    }
```

Then run the app using cabal

```bash
$ cabal run
```
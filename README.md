# static-api-generator
[![Build Status](https://travis-ci.org/jgalat/static-api-generator.svg?branch=master)](https://travis-ci.org/jgalat/static-api-generator) [![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

A Haskell DSL for writing static JSON APIs. This is a work in progress.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Web.StaticAPI

data Videogame = Videogame  { title     :: String
                            , publisher :: String
                            , year      :: String
                            }

instance ToJSON Videogame where
  toJSON vg = object [ "title"      .= title vg
                     , "publisher"  .= publisher vg
                     , "year"       .= year vg
                     ]

db :: [Videogame]
db = [ Videogame "Super Mario Bros." "Nintendo" "1985"
     , Videogame "The legend of Zelda" "Nintendo" "1986"
     , Videogame "Metal Gear" "Konami" "1987"
     ]

ps = map publisher db
ys = map year db

videogamesAPI :: StaticAPI
videogamesAPI =
  let gamesRoot = constant "games"
  in do
    -- "/games"
    route gamesRoot $ \_ ->
      db
    -- "/games/publisher/:name"
    route (gamesRoot </> constant "publisher" </> variable "name" ps) $ \e ->
      let p = get "name" e
      in filter (\vg -> publisher vg == p) db
    -- "/games/year/:year"
    route (gamesRoot </> constant "year" </> variable "year" ys) $ \e ->
      let y = get "year" e
      in filter (\vg -> year vg == y) db

main :: IO ()
main = staticAPI videogamesAPI defaultOpts
```

The example above would generate the following directory tree:

```
output
└── games
    ├── index.html
    ├── publisher
    │   ├── Konami
    │   │   └── index.html
    │   └── Nintendo
    │       └── index.html
    └── year
        ├── 1985
        │   └── index.html
        ├── 1986
        │   └── index.html
        └── 1987
            └── index.html
```

You can then host your API with services like [Github Pages](https://pages.github.com/).

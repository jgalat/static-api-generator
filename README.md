<h1 align="center">
  <img alt="static-api-generator" src=".github/logo.jpg">
</h1>

[![Build](https://github.com/jgalat/static-api-generator/actions/workflows/build.yml/badge.svg)](https://github.com/jgalat/static-api-generator/actions/workflows/build.yml)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

A Haskell DSL for writing static APIs.

```haskell
{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
import           Data.Aeson
import           Data.Text (Text)
import           Network.API.StaticAPI

default (Text)

data Videogame = Videogame
    { title     :: Text
    , publisher :: Text
    , year      :: Text
    }

instance ToJSON Videogame where
    toJSON vg = object
        [ "title"      .= title vg
        , "publisher"  .= publisher vg
        , "year"       .= year vg
        ]

db :: [Videogame]
db = [ Videogame "Super Mario Bros." "Nintendo" "1985"
     , Videogame "The legend of Zelda" "Nintendo" "1986"
     , Videogame "Metal Gear" "Konami" "1987"
     ]

videogamesAPI :: StaticAPI ()
videogamesAPI =
    let publishers = map publisher db
        years      = map year db
    in prefix "games" $ do
        -- "/games"
        root (return (toJSON db))

        -- "/games/publisher/:name"
        route ("publisher" ./ "name" .> publishers) $ do
            p <- getPathSegment "name"
            return (toJSON (filter (\vg -> publisher vg == p) db))

        -- "/games/year/:year"
        route ("year" ./ "year" .> years) $ do
            y <- getPathSegment "year"
            return (toJSON (filter (\vg -> year vg == y) db))

main :: IO ()
main = staticAPI videogamesAPI
```

The example above will generate the following directory tree:

```
public
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

```bash
$ cat public/games/index.html | jq
[
  {
    "year": "1985",
    "title": "Super Mario Bros.",
    "publisher": "Nintendo"
  },
  {
    "year": "1986",
    "title": "The legend of Zelda",
    "publisher": "Nintendo"
  },
  {
    "year": "1987",
    "title": "Metal Gear",
    "publisher": "Konami"
  }
]
```

```bash
$ cat public/games/year/1986/index.html | jq
[
  {
    "year": "1986",
    "title": "The legend of Zelda",
    "publisher": "Nintendo"
  }
]
```

```bash
$ cat public/games/publisher/Nintendo/index.html | jq
[
  {
    "year": "1985",
    "title": "Super Mario Bros.",
    "publisher": "Nintendo"
  },
  {
    "year": "1986",
    "title": "The legend of Zelda",
    "publisher": "Nintendo"
  }
]
```

You can then host your API with services like [Github Pages](https://pages.github.com/) or [Netlify](https://www.netlify.com/).

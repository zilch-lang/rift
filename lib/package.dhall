let Source =
      ./source.dhall
        sha256:ffaf30bb1622a6263e063a95de730c38d44c235ebe540052d7b30c750404e4b4

in  { Type =
        { -- | The name of the package
          name : Text
        , -- | Where to fetch the source of the package from
          src : Source
        , -- | If multiple components are declared in the source, specify one
          component : Optional Text
        , -- | A list of maintainers
          maintainers : List Text
        , -- | Is the package currently broken?
          broken : Bool
        , -- | Is the package deprecated?
          deprecated : Bool
        }
    , default =
      { component = None Text
      , maintainers = [] : List Text
      , broken = False
      , deprecated = False
      }
    }

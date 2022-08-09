let Source =
      ./source.dhall
        sha256:ffaf30bb1622a6263e063a95de730c38d44c235ebe540052d7b30c750404e4b4

let Version =
      ./version.dhall
        sha256:b93b52f5c05c797b2431149bfc17f26f8d30c1cae9a0ba43edd94e593969d564

in  { Type =
        { -- | The name of the package
          name : Text
        , -- |
          version : Version.Type
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

let Source =
      ./source.dhall
        sha256:ffaf30bb1622a6263e063a95de730c38d44c235ebe540052d7b30c750404e4b4

let Version =
      ./version.dhall
        sha256:596564e58f0959e1cccfa1bb154948adb195d9220d381f0742f4058c9d083b58

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

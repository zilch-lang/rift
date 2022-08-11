let Source =
      ./source.dhall
        sha256:ffaf30bb1622a6263e063a95de730c38d44c235ebe540052d7b30c750404e4b4

let Version =
      ./version.dhall
        sha256:54a70816c7185df69cd1d1a36410cc57a496e78d68422bc9d4332b6801d24275

in  { Type =
        { -- | The name of the package
          name : Text
        , -- |
          version : Version.Type
        , -- | Where to fetch the source of the package from
          src : Source
        }
    , default = {=}
    }

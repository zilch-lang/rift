let Source =
      ./source.dhall
        sha256:8fc3205ee05bdee3f8e4f553c96f898c14f7e9cb1a2150345cd7316694c4f8cb

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
        , -- | Is the package depreciated?
          depreciated : Bool
        }
    , default =
      { component = None Text
      , maintainers = [] : List Text
      , broken = False
      , depreciated = False
      }
    }

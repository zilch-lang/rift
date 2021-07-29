let VersionRange = { package : Text, range : Text }

let Component =
      { name : Text, source-dirs : List Text, kind : < Executable | Library > }

let Project =
      { Type =
          { name : Text
          , version : Text
          , dependencies : List VersionRange
          , components : List Component
          }
      , default =
        { dependencies = [] : List VersionRange
        , components = [] : List Component
        }
      }

in  { Project, VersionRange, Component }

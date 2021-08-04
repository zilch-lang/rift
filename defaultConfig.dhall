let VersionRange = { package : Text, range : Text }

let Component =
      { name : Text, source-dirs : List Text, kind : < Executable | Library > }

let Dependency =
      < Git : { url : Text, rev : Text, sha256 : Text }
      | Tar : { url : Text, sha256 : Text }
      >

let Project =
      { Type =
          { name : Text
          , version : Text
          , dependencies : List VersionRange
          , components : List Component
          , lts : Text
          , extra-deps : List Dependency
          }
      , default =
        { dependencies = [] : List VersionRange
        , components = [] : List Component
        , extra-deps = [] : List Dependency
        }
      }

in  { Project, VersionRange, Component, Dependency }

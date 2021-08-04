let VersionRange = { package : Text, range : Text }

let Component =
      let K = < Executable | Library >

      in  { Kind = K
          , Type =
              { name : Text
              , version : Text
              , dependencies : List VersionRange
              , source-dirs : List Text
              , kind : K
              }
          , default =
            { dependencies = [] : List VersionRange, source-dirs = [ "src" ] }
          }

let Dependency =
      < Git : { url : Text, rev : Text, sha256 : Text }
      | Tar : { url : Text, sha256 : Text }
      >

let Project =
      { Type =
          { components : List Component.Type
          , lts : Text
          , extra-deps : List Dependency
          }
      , default =
        { components = [] : List Component.Type
        , extra-deps = [] : List Dependency
        }
      }

in  { Project, VersionRange, Component, Dependency }

let VersionRange = { package : Text, range : Text }

let Component =
      let K = < Executable | Library >

      in  { Kind = K
          , Type =
              { -- The name of the component.
                name : Text
              , version : Text
              , dependencies : List VersionRange
              , -- A list of directories containing Zilch source files.
                -- When the component is for an executable, at least one of the files
                -- should be a main module.
                source-dirs : List Text
              , kind : K
              , -- Additional flags to give to the Zilch compiler.
                flags : List Text
              }
          , default =
            { dependencies = [] : List VersionRange
            , source-dirs = [ "src" ]
            , flags = [] : List Text
            }
          }

let Dependency =
      < Git : { url : Text, rev : Text, sha256 : Text }
      | Tar : { url : Text, sha256 : Text }
      >

let Project =
      { Type =
          { components : List Component.Type
          , -- The LTS version to use for the package set.
            lts : Text
          , -- Additional dependencies which are not part of the package set.
            extra-deps : List Dependency
          }
      , default =
        { components = [] : List Component.Type
        , extra-deps = [] : List Dependency
        }
      }

in  { Project, VersionRange, Component, Dependency }

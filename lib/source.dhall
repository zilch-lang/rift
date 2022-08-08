let Location =
      ./location.dhall
        sha256:613ebb491aeef4ff06368058b4f0e6e3bb8a58d8c145131fc0b947aac045a529

in  < Git :
        { -- | The URL of the git repository
          url : Location
        , -- | The revision to pull the source code from (can be a branch name e.g. @master@ or a tag e.g. @v1.0.0@)
          rev : Text
        }
    | Tar :
        { -- | The URL of the @.tar@ file
          url : Location
        , -- | The SHA-256 checksum of the file
          sha256 : Text
        }
    | TarGz :
        { -- | The URL of the @.tar@ file
          url : Location
        , -- | The SHA-256 checksum of the file
          sha256 : Text
        }
    | Zip :
        { -- | The URL of the @.tar@ file
          url : Location
        , -- | The SHA-256 checksum of the file
          sha256 : Text
        }
    >

let Ty = < Unstable | Stable : { major : Natural, minor : Natural } >

in  { Type = Ty
    , unstable = Ty.Unstable
    , stable =
        λ(major : Natural) → λ(minor : Natural) → Ty.Stable { major, minor }
    }

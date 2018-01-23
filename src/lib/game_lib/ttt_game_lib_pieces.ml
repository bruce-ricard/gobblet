
module XOPiece =
  struct
    type t = [ `X | `O ]
    let pieces = [ `X ; `O ]
    let serialize = function
      | `X -> 'x'
      | `O -> 'o'

    let deserialize = function
      | 'x' -> Some `X
      | 'o' -> Some `O
      | _ -> None

  end

module XPiece =
  struct
    type t = [ `X ]
    let pieces = [ `X ]
    let serialize = function
      | `X -> 'x'

    let deserialize = function
      | 'x' -> Some `X
      | _ -> None
  end

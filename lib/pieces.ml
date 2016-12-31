open Types

module XOPiece =
  struct
    type t = [ `X | `O ]
    let pieces = [ `X ; `O ]
  end

module XPiece =
  struct
    type t = [ `X ]
    let pieces = [ `X ]
  end

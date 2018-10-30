open Engine

module Game =
  struct
    type piece = unit
    type t = (bool * (bool * bool))
    type square = int
    type move = square
    type board = square -> piece option
    type move_result =
      [
      | `Ok of t
      | `Invalid
      ]
    let result _ = Won
    let all_moves _ = [1;2;3]
    let move (g : t) (m : move) (p : player) : move_result =
      match g,m with
      | (false, (b, c)), 1 -> `Ok (true, (b, c))
      | (a, (false, c)), 2 -> `Ok (a, (true, c))
      | (a, (b, false)), 3 -> `Ok (a, (b, true))
      | _ -> `Invalid

    let takeback g = ()
    let evaluate g p =
      match g with
      | (true, _) -> Win(0)
      | (false, (true, true)) -> Draw(0)
      | _ -> Unclear(0.)
  end

module DumbEngine = Engine.Make(Game)

let engine_unit_suite = [

  ]

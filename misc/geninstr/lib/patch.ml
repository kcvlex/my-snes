type frag = Lib.instr -> Lib.instr

type t = {
  f : Lib.instr -> Lib.instr
}

let make_patch fl =
  let f = List.fold_left (fun acc f -> (fun x -> f (acc x))) (fun x -> x) fl in
  { f }

let apply p t = p.f t

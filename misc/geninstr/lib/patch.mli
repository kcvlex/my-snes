type t

type frag = Lib.instr -> Lib.instr

val make_patch : frag list -> t

val apply: t -> Lib.instr -> Lib.instr

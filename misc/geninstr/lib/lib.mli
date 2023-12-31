type instr = {
  code : int;
  op : string;
  addr_mode : string;
}

val read_tables : string -> string array list

val to_instr : string array -> instr

open Str

type instr = {
  code : int;
  op : string;
  addr_mode : string;
}

type loader_type = {
  lines : string array list;
  chan : in_channel;
  is_table : bool;
}

let split_spaces line = split (regexp "[ \t]+") line

let table_begin = [
  "OP";
  "LEN";
  "CYCLES";
  "MODE";
  "nvmxdizc";
  "e";
  "SYNTAX"
]

let is_table_top lis =
  let rec aux lis1 lis2 = match (lis1, lis2) with
    | ([], []) -> true
    | (x1 :: xs1, x2 :: xs2) when x1 = x2 -> aux xs1 xs2
    | _ -> false
  in
  aux lis table_begin

let rec read_tables_aux loader =
  let { lines; chan; is_table } = loader in
  try
    let words = chan |> input_line |> split_spaces in
    let loader =
      if is_table then
        let words = Array.of_list words in
        if Array.length words = 0 then
          {
            lines = lines;
            chan = chan;
            is_table = false;
          }
        else
          {
            lines = words :: lines;
            chan = chan;
            is_table = true;
          }
      else
        if is_table_top words then
          let _ = input_line chan in
          {
            lines = lines;
            chan = chan;
            is_table = true;
          }
        else
          { lines; chan; is_table }
    in
    read_tables_aux loader
  with End_of_file -> begin
    assert (not is_table);
    List.rev loader.lines
  end

let read_tables filepath =
  let chan = open_in filepath in
  let loader = {
    lines = [];
    chan;
    is_table = false;
  }
  in
  let res = read_tables_aux loader in
  close_in chan; res

let convert_addressing_mode s = match s with
  | "abs" -> "ABS"
  | "abs,X" -> "ABS_X"
  | "abs,Y" -> "ABS_Y"
  | "(abs)" -> "ABS_IND"
  | "[abs]" -> "ABS_IND_LONG"
  | "(abs,X)" -> "ABS_IND_X"
  | "acc" -> "ACC"
  | "dir" -> "DIR"
  | "dir,X" -> "DIR_X"
  | "dir,Y" -> "DIR_Y"
  | "(dir)" -> "DIR_IND"
  | "[dir]" -> "DIR_IND_LONG"
  | "(dir,X)" -> "DIR_X_IND"
  | "(dir),Y" -> "DIR_IND_Y"
  | "[dir],Y" -> "DIR_IND_LONG_Y"
  | "imm" -> "IMM"
  | "imp" -> "IMPLIED"
  | "long" -> "LONG"
  | "long,X" -> "LONG_X"
  | "rel8" -> "REL8"
  | "rel16" -> "REL16"
  | "src,dest" -> "SRC_DST"
  | "stk,S" -> "STK_S"
  | "(stk,S),Y" -> "STK_S_Y"
  | _ -> raise (Invalid_argument s)

(* let convert_opcode s = "Opcode::" ^ s *)
let convert_opcode s = s

let to_instr arr =
  let code = arr.(0) |> Printf.sprintf "0x%s" |> int_of_string in
  let addr_mode = convert_addressing_mode arr.(3) in
  let op = convert_opcode arr.(6) in
  { code; addr_mode; op }

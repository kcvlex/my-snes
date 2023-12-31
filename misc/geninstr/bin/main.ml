open Geninstr
open Geninstr.Lib

let template1 = format_of_string "\
macro_rules! opcode_to_ident {\n\
  ($macro: ident, $code: expr) => {\n\
    match $code {\n\
      %s\n\
    }\n\
  }\n\
}\n\
pub(crate) use opcode_to_ident;
"

let template2 = format_of_string "\
macro_rules! ident_to_opcode {\n\
  %s\n\
}\n\
pub(crate) use ident_to_opcode;
"

let frag0 x =
  if x.code = 0x62 then
    {
      code = x.code;
      op = x.op;
      addr_mode = "REL16"
    }
  else
    x

let patch = Patch.make_patch [ frag0 ]

let () =
  let filepath = Sys.argv.(1) in
  let data = Lib.read_tables filepath in
  let data = 
    data
      |> List.map Lib.to_instr
      |> List.map (Patch.apply patch)
      |> List.sort (fun x y -> x.code - y.code)
  in
  let code1 =
    data
      |> List.map (fun x -> Printf.sprintf "0x%02X => $macro!(%s, %s)," x.code x.op x.addr_mode)
      |> String.concat "\n"
      |> Printf.sprintf template1
  in
  let code2 =
    data
      |> List.map (fun x -> Printf.sprintf "(%s, %s) => { 0x%02X };" x.op x.addr_mode x.code)
      |> String.concat "\n"
      |> Printf.sprintf template2
  in
  Printf.printf "%s\n\n%s" code1 code2

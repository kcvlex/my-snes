use std::fs::File;
use std::io::{ self, BufRead };

fn operand(s: &str) -> &str {
    match s {
        "A" => "A",
        "X" => "X",
        "Y" => "Y",
        "SP" => "SP",
        "YA" => "YA",
        "PSW" => "PSW",
        "C" => "C",

        "#imm" => "imm",
        "dp" => "dp",
        "(X)" => "X ind",
        "(Y)" => "Y ind",
        "(X)+" => "X ind inc",
        "dp+X" => "dp X p",
        "dp+Y" => "dp Y p",
        "!abs" | "abs" => "abs",
        "rel" => "rel",
        "[dp+X]" => "dp X p ind",
        "[dp]+Y" => "dp ind Y p",
        "!abs+X" => "abs X p",
        "!abs+Y" => "abs Y p",
        "[!abs+X]" => "abs X p ind",
        "bit" => "bit",
        "up" => "up",
        "aaa.b" => "aaab",
        "/aaa.b" => "not aaab",
        _ => panic!("unknown string: {}", s),
    }
}

const CB: &str = "$cb";

fn call(args: &[&str]) -> String {
    format!("{{ {}!({}) }}", CB, args.join(", "))
}

fn parse_line(line: &str) -> Vec<(u8, String)> {
    let (instr, opcode) = {
        let v: Vec<&str> = line.split(" ").collect();
        (v[0], v[1])
    };

    let mut res = Vec::new();
    match (opcode, instr) {
        ("n1", "TCALL,n") => {
            for i in 0x0..=0xF {
                let opcode = i * 0x10 + 1;
                let arm = call(&vec!["TCALL", &i.to_string()]);
                res.push((opcode, arm));
            }
        },
        ("x2", "SET1,dp,bit") => {
            for bit in 0x0..0x8 {
                let x = bit * 2;
                let opcode = x * 0x10 + 2;
                let arm = call(&vec!["SET1", "dp", &bit.to_string()]);
                res.push((opcode, arm));
            }
        },
        ("y2", "CLR1,dp,bit") => {
            for bit in 0x0..0x8 {
                let y = bit * 2 + 1;
                let opcode = y * 0x10 + 2;
                let arm = call(&vec!["CLR1", "dp", &bit.to_string()]);
                res.push((opcode, arm));
            }
        },
        ("x3", "BBS,dp,bit,rel") => {
            for bit in 0x0..0x8 {
                let x = bit * 2;
                let opcode = x * 0x10 + 3;
                let arm = call(&vec!["BBS", "dp", &bit.to_string(), "rel"]);
                res.push((opcode, arm));
            }
        },
        ("y3", "BBC,dp,bit,rel") => {
            for bit in 0x0..0x8 {
                let y = bit * 2 + 1;
                let opcode = y * 0x10 + 3;
                let arm = call(&vec!["BBC", "dp", &bit.to_string(), "rel"]);
                res.push((opcode, arm));
            }
        },
        (opcode, _) => {
            let mut instr: Vec<&str> = instr.split(",").collect();
            for i in 1..instr.len() {
                instr[i] = operand(instr[i]);
            }
            let opcode = u8::from_str_radix(opcode, 16).unwrap();
            let arm = call(&instr);
            res.push((opcode, arm));
        },
    }

    res
}

fn read_lines(filename: &str) -> Vec<String> {
    // Open the file in read-only mode.
    let file = File::open(filename).unwrap(); 
    // Read the file line by line, and return an iterator of the lines of the file.
    io::BufReader::new(file).lines().map(|x| x.unwrap()).collect() 
}

fn run(lines: Vec<String>) -> String {
    let mut v = Vec::new();
    let indent = "    ".repeat(3);
    for line in lines.iter() {
        for (opcode, arm) in parse_line(&line[..]).iter() {
            v.push(format!("{}{:#04X} => {}", indent, opcode, arm));
        }
    }

    format!("// Generated code
macro_rules! decode {{
    ($opcode: expr, {}: ident) => {{
        match $opcode {{
{}
        }}
    }}
}}

pub(crate) use decode;
    ", CB, v.join(",\n"))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let res = run(read_lines(&args[1])).to_string();
    println!("{}", res);
}

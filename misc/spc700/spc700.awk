#!/bin/awk -f
BEGIN {
    FS="\t";
}
{
    instr=$1;
    opcode=$3;
    gsub(" ", ",", instr);
    gsub(",+", ",", instr);
    sub(",$", "", instr);
    print(instr, opcode)
}

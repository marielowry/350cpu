module control(insn, opcode, rd, rs, rt, shamt, ALUop, imm, target);
// input: instruction
// outputs: a lot
    input [31:0] insn;

    output [4:0] opcode, rd, rs, rt, shamt, ALUop;
    output [31:0] imm;
    output [26:0] target;

    wire [16:0] imm_unextended;
    
    assign opcode = insn[31:27];
    assign rd = insn[26:22];
    assign rs = insn[21:17];
    assign rt = insn[16:12];
    assign shamt = insn[11:7];
    assign ALUop = insn[6:2];

    assign imm_unextended = insn[16:0];

    assign target = insn[26:0];

    assign imm = {{15{imm_unextended[16]}}, {imm_unextended}};


endmodule
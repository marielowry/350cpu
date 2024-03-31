module alu(data_operandA, data_operandB, ctrl_ALUopcode, ctrl_shiftamt, data_result, isNotEqual, isLessThan, overflow);

    input [31:0] data_operandA, data_operandB;
    input [4:0] ctrl_ALUopcode, ctrl_shiftamt;

    output [31:0] data_result;
    output isNotEqual, isLessThan, overflow;

    wire [31:0] ADD, SUB, AND, OR, SLL, SRA;
    wire ADD_ovf, SUB_ovf;
    wire [6:0] decoderOutput;
    wire notOpCode_0, notOpCode_1, notOpCode_2, notOpCode_3, notOpCode_4, notOpCode_5;
    wire notEqAdder, lessThanAdder, noOps;

    // do all 6 calculations in parallel
    addCalc ADD_AB(.A(data_operandA), .B(data_operandB), .S(ADD), .isNotEqual(notEqAdder), .isLessThan(lessThanAdder), .overflow(ADD_ovf), .subTrue({1'b0})); 
    addCalc SUB_AB(.A(data_operandA), .B(data_operandB), .S(SUB), .isNotEqual(isNotEqual), .isLessThan(isLessThan), .overflow(SUB_ovf), .subTrue({1'b1}));
    // TODO: this feels silly to calc both def check this
    andCalc AND_AB(.A(data_operandA), .B(data_operandB), .result(AND));
    orCalc OR_AB(.A(data_operandA), .B(data_operandB), .result(OR));
    sllCalc SLL_AB(.A(data_operandA), .B(data_operandB), .shiftamt(ctrl_shiftamt), .result(SLL));
    sraCalc SRA_AB(.A(data_operandA), .B(data_operandB), .shiftamt(ctrl_shiftamt), .result(SRA));

    // determine which overflow bits to use with 2:1 mux, calculation only correct for add or subtract
    assign overflow = ctrl_ALUopcode[0] ? SUB_ovf : ADD_ovf;

    // *** decoder logic ***

    // first, get inverted wires for all opcode inputs
    not InvOp0(notOpCode_0, ctrl_ALUopcode[0]);
    not InvOp1(notOpCode_1, ctrl_ALUopcode[1]);
    not InvOp2(notOpCode_2, ctrl_ALUopcode[2]);
    not InvOp3(notOpCode_3, ctrl_ALUopcode[3]);
    not InvOp4(notOpCode_4, ctrl_ALUopcode[4]);

    // ADD: 00000, decoderOutput[0] = 1 for to compute add operation
    and doADD(decoderOutput[0], notOpCode_4, notOpCode_3, notOpCode_2, notOpCode_1, notOpCode_0); 

    // SUBTRACT: 00001
    and doSUB(decoderOutput[1], notOpCode_4, notOpCode_3, notOpCode_2, notOpCode_1, ctrl_ALUopcode[0]); 

    // AND: 00010
    and doAND(decoderOutput[2], notOpCode_4, notOpCode_3, notOpCode_2, ctrl_ALUopcode[1], notOpCode_0); 

    // OR: 00011
    and doOR(decoderOutput[3], notOpCode_4, notOpCode_3, notOpCode_2, ctrl_ALUopcode[1], ctrl_ALUopcode[0]); 

    // SLL: 00100
    and doSLL(decoderOutput[4], notOpCode_4, notOpCode_3, ctrl_ALUopcode[2], notOpCode_1, notOpCode_0); 

    // SRL: 00101
    and doSRL(decoderOutput[5], notOpCode_4, notOpCode_3, ctrl_ALUopcode[2], notOpCode_1, ctrl_ALUopcode[0]); 

    // all other insns
    //assign decoderOutput[6] = ((ctrl_ALUopcode[4:2] != 3'b000) || (ctrl_ALUopcode != 5'b00101) || (ctrl_ALUopcode != 5'b00100) ) ? 1'b1 : 1'b0;
    assign decoderOutput[6] = ((ctrl_ALUopcode == 5'b00110) || (ctrl_ALUopcode == 5'b00111)) ? 1'b1 : 1'b0;

    // assign data_result based on decoder output
    assign data_result = decoderOutput[0] ? ADD : {(32){1'bz}};
    assign data_result = decoderOutput[1] ? SUB : {(32){1'bz}};
    assign data_result = decoderOutput[2] ? AND : {(32){1'bz}};
    assign data_result = decoderOutput[3] ? OR : {(32){1'bz}};
    assign data_result = decoderOutput[4] ? SLL : {(32){1'bz}};
    assign data_result = decoderOutput[5] ? SRA : {(32){1'bz}};
    assign data_result = decoderOutput[6] ? {(32){1'b0}} : {(32){1'bz}};



endmodule
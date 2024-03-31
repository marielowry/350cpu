/**
 * READ THIS DESCRIPTION!
 *
 * This is your processor module that will contain the bulk of your code submission. You are to implement
 * a 5-stage pipelined processor in this module, accounting for hazards and implementing bypasses as
 * necessary.
 *
 * Ultimately, your processor will be tested by a master skeleton, so the
 * testbench can see which controls signal you active when. Therefore, there needs to be a way to
 * "inject" imem, dmem, and regfile interfaces from some external controller module. The skeleton
 * file, Wrapper.v, acts as a small wrapper around your processor for this purpose. Refer to Wrapper.v
 * for more details.
 *
 * As a result, this module will NOT contain the RegFile nor the memory modules. Study the inputs 
 * very carefully - the RegFile-related I/Os are merely signals to be sent to the RegFile instantiated
 * in your Wrapper module. This is the same for your memory elements. 
 *
 *
 */

// 32 bit processor!

// *** QUESTIONS ***

// *** TO DO ***
    // Ensure that 0 register can only be set to 0, and that it will always stay at 0
    // don't increment PC when NOP inserted
    // check insn for bypassing actually writes to rd register (not sw, bne, blt)


module processor(
    // Control signals
    clock,                          // I: The master clock
    reset,                          // I: A reset signal

    // Imem
    address_imem,                   // O: The address of the data to get from imem
    q_imem,                         // I: The data from imem

    // Dmem
    address_dmem,                   // O: The address of the data to get or put from/to dmem
    data,                           // O: The data to write to dmem
    wren,                           // O: Write enable for dmem
    q_dmem,                         // I: The data from dmem

    // Regfile
    ctrl_writeEnable,               // O: Write enable for RegFile
    ctrl_writeReg,                  // O: Register to write to in RegFile
    ctrl_readRegA,                  // O: Register to read from port A of RegFile
    ctrl_readRegB,                  // O: Register to read from port B of RegFile
    data_writeReg,                  // O: Data to write to for RegFile
    data_readRegA,                  // I: Data from port A of RegFile
    data_readRegB                   // I: Data from port B of RegFile
	 
	);

	// Control signals
	input clock, reset;
	
	// Imem
    output [31:0] address_imem;
	input [31:0] q_imem;

	// Dmem
	output [31:0] address_dmem, data;
	output wren;
	input [31:0] q_dmem;

	// Regfile
	output ctrl_writeEnable;
	output [4:0] ctrl_writeReg, ctrl_readRegA, ctrl_readRegB;
	output [31:0] data_writeReg;
	input [31:0] data_readRegA, data_readRegB;

    // wires to connect components
    wire [31:0] reg_outA, reg_outB;
    wire [31:0] DX_LATCH_IN_A, DX_LATCH_IN_B, DX_LATCH_A, DX_LATCH_B, DX_LATCH_IR_out, DX_IR_in;
    wire [31:0] ALU_out, DMEM_out, regfile_input, ALU_opB;
    wire [31:0] XM_LATCH_O, XM_LATCH_B, XM_LATCH_IR, MW_LATCH_IR, XM_LATCH_IR_in, XM_LATCH_PC_out;

    // PC CONTROL WIRES
    wire [31:0] pc_reg_in, pc_reg_out, imem_out, pc_increment_amt, DX_LATCH_PC_out, pc_input; // PC wires
    wire pc_increment_last, x;

    // FD LATCH WIRES
    wire [31:0] pc_FDlatch_in, pc_FDlatch_out, IR_FD, FD_IR_IN, IR_FD_1;

    // PW LATCH WIRES
    wire [31:0] PW_LATCH_IR_out;

    //control wires
    wire [4:0] opcode, rd, rs, rt, shamt, ALUop;
    wire [31:0] imm;
    wire [26:0] target;

    // control after dx
    wire [4:0] opcode_dx, rd_dx, rs_dx, rt_dx, shamt_dx, ALUop_dx, aluCode;
    wire [31:0] imm_dx;

    wire addi_true;

    // MULTDIV wires
    wire op_mult, op_div, multdiv_resultRDY, multdiv_exception, mult_op, div_op, multdiv_resultRDY_pw;
    wire [31:0] multdiv_result, multdiv_result_pw, multdiv_result_exc_pw;
    wire [5:0] mult_counter, div_counter;

    // sw and lw wires
    wire [31:0] lw_mem_out;
    wire decode_sw_true, sw_true, lw_true, xm_lw_true, mw_sw_true, mw_lw_true;

    // j insn wires
    wire [31:0] jump_addr, jr_addr, jal_addr;
    wire DX_jump_true, DX_jr_true, DX_jal_true, decode_jr_true, decode_jump_true, decode_jal_true;

    // branch insn wires
    wire [31:0] branch_addr;
    wire decode_bne_true, decode_blt_true, DX_bne_true, DX_blt_true, alu_bne_true, bne_taken, blt_taken, alu_lt, branch_taken;

    // bex and setx wires
    wire [31:0] mw_target_extended, mw_alu_result_exc, pw_writedata, rstatus_exceptions, MW_LATCH_PC_out;
    wire mw_setx_true, DX_bex_true, decode_bex_true, alu_overflow, zero_opcode, x_add_op, x_sub_op, x_mul_op, x_div_op, alu_overflow_xm, alu_overflow_mw, pw_multdiv_exception, mw_multdiv_exception;
    wire x_addi_op, multReady, divReady, pw_multReady, pw_divReady, mw_not_nop, mw_jal_true, DX_lw_true;
    wire [4:0] mw_writereg, mw_exc_writereg, pw_writereg, xm_writereg;

    // bypassing wires
    wire [31:0] rt_ALU_opB, ALU_opA, DMEM_data, xm_alu_result_exc;
    wire rs1_bypassOpTrue, rs2_bypassOpTrue, rs1_eq_xmOut, rs1_eq_mwOut, rs2_eq_xmOut, rs2_eq_mwOut, dx_rdWritten_xm, dx_rdWritten_mw, xm_sw_true, xm_sw_bypass;
    wire xm_not_nop, xm_add_op, xm_sub_op, xm_addi_op, xm_rdWritten_mw_alu, xm_rdWritten_mw_lw, rs1_notZero, rs2_notZero;
    wire dx_rd_eq_xmOut, dx_rd_eq_mwOut, rd_notZero, mw_jr_true;

    // stall wires
    wire fd_lw_dependency, fd_lw_stall, fd_lw_stall_last, op_mult_stable, op_div_stable, mult_op_fd, div_op_fd, multdiv_stall, multdiv_resultRDY_mw;
    wire [31:0] xm_bypass_input, mw_bypass_input, multdiv_result_mw;
    wire [4:0] ctrl_readRegA_dx, ctrl_readRegB_dx, mw_multdiv_writereg;
    wire multdiv_resultRDY_pw_check;

// **** PC **** 
    // register to store pc, 32 bits, this can be normal one that is rising edge triggered
    // latch after this stage stores pc
    // use adder to increment pc (delay doesn't matter because of pipelining)
    // register to store pc


    addCalc newAddr(.A(DX_LATCH_PC_out), .B(imm_dx), .S(branch_addr), .isNotEqual(), .isLessThan(), .overflow(), .subTrue(1'b0));
    // mux to select between jump insn output and incremented pc
    assign pc_input = (bne_taken || blt_taken) ? branch_addr : ((DX_jump_true || DX_jal_true || DX_bex_true) ? jump_addr : (DX_jr_true ? jr_addr : pc_reg_in));

    //regOne pc_register(.data(pc_input), .clk(~clock), .reset(reset), .enable((1'b1)), .out(pc_reg_out)); 
    regOne pc_register(.data(pc_input), .clk(~clock), .reset(reset), .enable(!multdiv_stall), .out(pc_reg_out)); // yyy
    // increment register by 1
    addCalc INCREMENT_PC(.A(pc_reg_out), .B(pc_increment_amt), .S(pc_reg_in), .isNotEqual(), .isLessThan(), .overflow(), .subTrue(1'b0));

    // if multiply instruction, don't increment pc until after the operation is complete, don't increment for 16 cycles?
    // change input to adder from 1 to 0

    // imem fetch: only get new pc if mult or div is not true


// **** IMEM **** 
    // this is ROM, OUTPUTS/INPUTS TO ELEMENTS IN WRAPPER MODULE
    assign address_imem = pc_reg_out;
    assign imem_out = q_imem;

    assign mult_op = (imem_out[6:2] == 5'b00110) && (imem_out[31:27] == 5'b00000);
    assign div_op = (imem_out[6:2] == 5'b00111) && (imem_out[31:27] == 5'b00000);
    // assign mult_op = op_mult_stable;
    // assign div_op = op_div_stable;

    // reset condition for counter: mult op is the current insn and the pc reg was previously incremented (first cycle of mult op) or counter at val
    //counter multcounter(mult_counter, clock, ((mult_op || div_op) && !(pc_reg_in == pc_reg_out)) || multdiv_resultRDY_pw);    
    //counter multcounter(mult_counter, clock, ((mult_op || div_op) && !(pc_reg_in == pc_reg_out)) || multdiv_resultRDY_pw);    


    // increment 0 if mult_op and mult_counter is not 16, same for div but 32

    // increment conditions: mult op is high, the result is not ready and last increment amount was 0
    //assign pc_increment_amt =  fd_lw_stall || (!multdiv_resultRDY_pw && ((mult_op || div_op) || (!fd_lw_stall_last && !pc_increment_last && (mult_counter != 6'b000000)))) ? 32'b0 : {{31'b0},{1'b1}};
    //assign pc_increment_amt =  fd_lw_stall || (!multdiv_resultRDY_pw && ((mult_op || div_op) || (!fd_lw_stall_last && !pc_increment_last && (mult_counter != 6'b000000)))) ? 32'b0 : {{31'b0},{1'b1}};
    assign pc_increment_amt = fd_lw_stall ? 32'b0 : {{31'b0},{1'b1}}; //yyy

    // dffe_ref (q, d, clk, en, clr);
    dffe_ref pc_increment_prev(pc_increment_last, pc_increment_amt[0], clock, 1'b1, reset);
    dffe_ref fd_lw_stall_prev(fd_lw_stall_last, fd_lw_stall, clock, 1'b1, reset);


    // FOR FD LATCH IR IN: select imem_out if pc != prev pc, else select all 0s
    //assign FD_IR_IN = (((pc_reg_in == pc_reg_out) && (mult_counter != 6'b000000)) && !fd_lw_stall) || branch_taken ? 32'b0 : imem_out;
     assign FD_IR_IN = branch_taken ? 32'b0 : imem_out;


// **** F/D LATCH ****
    regFallingEdge FD_LATCH_PC(.data(pc_reg_in), .clk(clock), .reset(reset), .enable(!fd_lw_stall && !multdiv_stall), .out(pc_FDlatch_out)); //yyy enable
    regFallingEdge FD_LATCH_IR(.data(FD_IR_IN), .clk(clock), .reset(reset), .enable(!fd_lw_stall && !multdiv_stall), .out(IR_FD_1)); 
    dffe_ref FD_LATCH_MULTOP(.q(mult_op_fd), .d(mult_op), .clk(~clock), .en(1'b1), .clr(reset));
    dffe_ref FD_LATCH_DIVOP(.q(div_op_fd), .d(div_op), .clk(~clock), .en(1'b1), .clr(reset));


    assign branch_taken = (DX_jump_true || DX_jal_true  || DX_jr_true || bne_taken || blt_taken || DX_bex_true);
    assign IR_FD =  branch_taken ? 32'b0 : IR_FD_1;

    control INSN_CONTROL(IR_FD, opcode, rd, rs, rt, shamt, ALUop, imm, target);

    and decode_sw_insn(decode_sw_true, !opcode[4], !opcode[3], opcode[2], opcode[1], opcode[0]);
    and decode_jump_insn(decode_jump_true, !opcode[4], !opcode[3], !opcode[2], !opcode[1], opcode[0]);
    and decode_jr_insn(decode_jr_true, !opcode[4], !opcode[3], opcode[2], !opcode[1], !opcode[0]);
    and decode_jal_insn(decode_jal_true, !opcode[4], !opcode[3], !opcode[2], opcode[1], opcode[0]);
    and decode_bne_insn(decode_bne_true, !opcode[4], !opcode[3], !opcode[2], opcode[1], !opcode[0]);
    and decode_blt_insn(decode_blt_true, !opcode[4], !opcode[3], opcode[2], opcode[1], !opcode[0]);
    and decode_bex_insn(decode_bex_true, opcode[4], !opcode[3], opcode[2], opcode[1], !opcode[0]);


// **** STALL LOGIC ****
    
    // wires: fd_lw_true, fd_lw_dependency, fd_lw_stall

    assign fd_lw_dependency = (IR_FD[21:17] == DX_LATCH_IR_out[26:22]) || ((IR_FD[16:12] == DX_LATCH_IR_out[26:22]) && (IR_FD != 5'b00111));
    // stall when lw insn, reading from write reg, and next insn is not a sw
    assign fd_lw_stall = DX_lw_true && fd_lw_dependency;



// **** REGISTER FILE ****
    // DONT HAVE AN INSTANCE OF THE REGFILE HERE!!
    // SEND TO OUTPUTS AND INPUTS IN WRAPPER MODULE

    // select correct input bit to register, pw vs mw latch output
    //assign ctrl_writeEnable = !mw_sw_true && (mw_jal_true || ((MW_LATCH_IR[31:27] == 5'b00000) && ((MW_LATCH_IR[6:4] == 3'b000) || (MW_LATCH_IR[6:4] == 4'b0010))) || (MW_LATCH_IR[31:27] == 5'b00101) || ((PW_LATCH_IR_out[6:3] == 4'b0011) && multdiv_resultRDY_pw)) ? 1'b1 : 1'b0; //&& (MW_LATCH_IR != 32'b0)
    assign ctrl_writeEnable = !mw_sw_true && !mw_jr_true && (mw_jal_true || (MW_LATCH_IR[6:4] == 3'b000) || (MW_LATCH_IR[6:3] == 4'b0010) || (MW_LATCH_IR[31:27] == 5'b00101) || ((MW_LATCH_IR[6:3] == 4'b0011) && multdiv_resultRDY_mw)) ? 1'b1 : 1'b0; //&& (MW_LATCH_IR != 32'b0)
    // enabled with NOP, but writes to zero register so not an issue
    assign ctrl_writeReg = mw_setx_true ? 5'b11110 : (mw_jal_true ? 5'b11111 : (mw_lw_true ? MW_LATCH_IR[26:22] : ((multdiv_resultRDY_mw && !pc_increment_last) ? mw_multdiv_writereg : mw_writereg))); //SIMPLIFY THIS
    assign ctrl_readRegA = decode_bex_true ? 5'b11110 : rs;
    assign ctrl_readRegB = (decode_sw_true || decode_jr_true || decode_bne_true || decode_blt_true) ? rd : rt; // get jr address from reg B read
    assign data_writeReg = mw_setx_true ? mw_target_extended : (mw_jal_true ? MW_LATCH_PC_out : (mw_lw_true ? lw_mem_out : ((multdiv_resultRDY_mw && !pc_increment_last) ? multdiv_result_mw : mw_alu_result_exc)));
    assign reg_outA = data_readRegA;
    assign reg_outB = data_readRegB; // read from rd if sw





// **** D/X LATCH ****

    // instantiate counter
    // regFallingEdge DX_LATCH_opA(.data(reg_outA), .clk(clock), .reset(reset), .enable(pc_increment_last || (mult_counter == 6'b000001)), .out(DX_LATCH_A));
    // regFallingEdge DX_LATCH_opB(.data(reg_outB), .clk(clock), .reset(reset), .enable(pc_increment_last || (mult_counter == 6'b000001)), .out(DX_LATCH_B));
    regFallingEdge DX_LATCH_opA(.data(reg_outA), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(DX_LATCH_A)); // yyy enable
    regFallingEdge DX_LATCH_opB(.data(reg_outB), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(DX_LATCH_B));

    assign DX_IR_in = (DX_jump_true || DX_jal_true  || DX_jr_true || bne_taken || blt_taken || DX_bex_true || fd_lw_stall) ? 32'b0 : IR_FD;
    regFallingEdge DX_LATCH_PC(.data(pc_FDlatch_out), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(DX_LATCH_PC_out));
    regFallingEdge DX_LATCH_IR(.data(DX_IR_in), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(DX_LATCH_IR_out));
    regFallingEdge_5bit DX_LATCH_opcode(.data(opcode), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(opcode_dx));
    regFallingEdge_5bit DX_LATCH_rd(.data(rd), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(rd_dx));
    regFallingEdge_5bit DX_LATCH_rs(.data(rs), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(rs_dx));
    regFallingEdge_5bit DX_LATCH_rt(.data(rt), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(rt_dx));
    regFallingEdge_5bit DX_LATCH_shamt(.data(shamt), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(shamt_dx));
    regFallingEdge_5bit DX_LATCH_ALUop(.data(ALUop), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(ALUop_dx));
    regFallingEdge DX_LATCH_imm(.data(imm), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(imm_dx));
    regFallingEdge_5bit DX_LATCH_readRegA(.data(ctrl_readRegA), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(ctrl_readRegA_dx));
    regFallingEdge_5bit DX_LATCH_readRegB(.data(ctrl_readRegB), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(ctrl_readRegB_dx));

    // stores operand A, operand B, PC, instruction, and control

    assign DX_jump_true = (DX_LATCH_IR_out[31:27] == 5'b00001);
    assign DX_jr_true = (DX_LATCH_IR_out[31:27] == 5'b00100);
    assign DX_jal_true = (DX_LATCH_IR_out[31:27] == 5'b00011);
    assign DX_bne_true = (DX_LATCH_IR_out[31:27] == 5'b00010);
    assign DX_blt_true = (DX_LATCH_IR_out[31:27] == 5'b00110);
    assign DX_bex_true = (DX_LATCH_IR_out[31:27] == 5'b10110) && (DX_LATCH_A != 32'b0);
    assign DX_lw_true = (DX_LATCH_IR_out[31:27] == 5'b01000);

    // address to jump to (see jr addr in bypass logic section)
    assign jump_addr = {{6{DX_LATCH_IR_out[26]}}, DX_LATCH_IR_out[25:0]};
    //assign jr_addr = DX_LATCH_B;
    //assign jr_addr = rt_ALU_opB;

    // if following insn or two insns write to rd, select different jr input



// **** BYPASS LOGIC ALU/MULTDIV INPUT ****

    // implement for read after write dependencies

    // conditions to check 
    // d/x latch source register 1 == x/m latch destination register
    // d/x latch source register 2 == x/m latch destination register
    // f/d latch source register 1 == m/w latch destination register
    // f/d latch source register 2 == m/w latch destination register
    // this is all one big or statement where a 1 indicates insert a nop

    // NOTE: ONLY IMPLEMENTED BYPASS LOGIC FOR NORMAL ALU OPS AND ADDI RN ADD OTHERS LATER 

    // D/X latch source 1: implement for opcode 00000, 00101 addi  
        // module mux_4(out, select, in0, in1, in2, in3);
        // mux select 01: d/x latch source reg 1 == x/m latch dest reg
        // mux select 10: d/x latch source reg 1 == m/w latch dest reg
        // mux select 00: else
        // mux select 11: pick xm output bc that will have correct result

    // rd is written to in alu ops, addi, lw
    assign dx_rdWritten_xm = (XM_LATCH_IR[31:27] == 5'b00000) || (XM_LATCH_IR[31:27] == 5'b00101) || (XM_LATCH_IR[31:27] == 5'b01000);
    assign dx_rdWritten_mw = (MW_LATCH_IR[31:27] == 5'b00000) || (MW_LATCH_IR[31:27] == 5'b00101) || (MW_LATCH_IR[31:27] == 5'b01000);

    assign rs1_notZero = (ctrl_readRegA_dx != 5'b00000);
    assign rs2_notZero = (ctrl_readRegB_dx != 5'b00000);
    assign rd_notZero = (rd_dx != 5'b00000);

    // if lw true, select output of dmem instead of output of alu
    // this applies to both xm and mw stage
    assign xm_bypass_input = xm_lw_true ? DMEM_out : xm_alu_result_exc ;
    assign mw_bypass_input = mw_lw_true ? lw_mem_out : mw_alu_result_exc ;


    // need to bypass jr when rd written in next two insns DX_jr_true
    // assign dx_rd_eq_xmOut = (rd_dx == xm_writereg) && DX_jr_true && rd_notZero;
    // assign dx_rd_eq_mwOut = (rd_dx == mw_writereg) && DX_jr_true && rd_notZero;
    assign dx_rd_eq_xmOut = (ctrl_readRegB_dx == xm_writereg) && DX_jr_true && rs2_notZero;
    assign dx_rd_eq_mwOut = (ctrl_readRegB_dx == mw_writereg) && DX_jr_true && rs2_notZero;

    mux_4 bypass_select_jr(jr_addr, {{dx_rd_eq_xmOut}, {dx_rd_eq_mwOut}}, DX_LATCH_B, mw_bypass_input, xm_bypass_input, xm_bypass_input); 



    assign rs1_bypassOpTrue = ((opcode_dx == 5'b00000) || (opcode_dx == 5'b00101) || (opcode_dx == 5'b00111) || (opcode_dx == 5'b01000)|| (opcode_dx == 5'b00110)|| (opcode_dx == 5'b00010));
    // assign rs1_eq_xmOut = rs1_bypassOpTrue && (rs_dx == xm_writereg) && dx_rdWritten_xm && rs1_notZero;
    // assign rs1_eq_mwOut = rs1_bypassOpTrue && (rs_dx == mw_writereg) && dx_rdWritten_mw && rs1_notZero; // below change reg to check
    assign rs1_eq_xmOut = rs1_bypassOpTrue && (ctrl_readRegA_dx == xm_writereg) && dx_rdWritten_xm && rs1_notZero;
    assign rs1_eq_mwOut = rs1_bypassOpTrue && (ctrl_readRegA_dx == mw_writereg) && dx_rdWritten_mw && rs1_notZero;

    // mux_4 bypass_select_rs1(ALU_opA, {{rs1_eq_xmOut}, {rs1_eq_mwOut}}, DX_LATCH_A, mw_alu_result_exc, xm_alu_result_exc, xm_alu_result_exc); 
    mux_4 bypass_select_rs1(ALU_opA, {{rs1_eq_xmOut}, {rs1_eq_mwOut}}, DX_LATCH_A, mw_bypass_input, xm_bypass_input, xm_bypass_input); 


    
    // D/X latch source 2: implement for opcode 00000
        // mux select 01: d/x latch source reg 2 == x/m latch dest reg
        // mux select 10: d/x latch source reg 2 == m/w latch dest reg
        // mux select 00: else

    assign rs2_bypassOpTrue = (opcode_dx == 5'b00000 && ALUop_dx!=5'b00100 && ALUop_dx!=5'b00101) || (opcode_dx == 5'b00110)|| (opcode_dx == 5'b00010);
    // assign rs2_eq_xmOut = rs2_bypassOpTrue && (rt_dx == xm_writereg) && rs2_notZero;
    // assign rs2_eq_mwOut = rs2_bypassOpTrue && (rt_dx == mw_writereg) && rs2_notZero;
    assign rs2_eq_xmOut = rs2_bypassOpTrue && (ctrl_readRegB_dx == xm_writereg) && rs2_notZero;
    assign rs2_eq_mwOut = rs2_bypassOpTrue && (ctrl_readRegB_dx == mw_writereg) && rs2_notZero;
    // mux_4 bypass_select_rs2(rt_ALU_opB, {{rs2_eq_xmOut}, {rs2_eq_mwOut}}, DX_LATCH_B, mw_alu_result_exc, xm_alu_result_exc, xm_alu_result_exc);
    mux_4 bypass_select_rs2(rt_ALU_opB, {{rs2_eq_xmOut}, {rs2_eq_mwOut}}, DX_LATCH_B, mw_bypass_input, xm_bypass_input, xm_bypass_input);





// **** ALU ****


    // and opcode 00101
    and addi_insn(addi_true, !opcode_dx[4], !opcode_dx[3], opcode_dx[2], !opcode_dx[1], opcode_dx[0]);
    and sw_insn(sw_true, !opcode_dx[4], !opcode_dx[3], opcode_dx[2], opcode_dx[1], opcode_dx[0]);
    and lw_insn(lw_true, !opcode_dx[4], opcode_dx[3], !opcode_dx[2], !opcode_dx[1], !opcode_dx[0]);

    // select immediate if addi or sw
    assign ALU_opB = (addi_true || sw_true || lw_true) ? imm_dx : rt_ALU_opB;
    assign aluCode = DX_blt_true ? 5'b00001 : ((addi_true || sw_true || lw_true) ? {5'b0} : ALUop_dx);


    alu ALU(.data_operandA(ALU_opA), .data_operandB(ALU_opB), .ctrl_ALUopcode(aluCode), 
    .ctrl_shiftamt(shamt_dx), .data_result(ALU_out), .isNotEqual(alu_bne_true), .isLessThan(alu_lt), .overflow(alu_overflow));
    // operand a from reg read port 1
    // operand b from reg read port 2

    // check for bne condition
    and take_bne(bne_taken, alu_bne_true, DX_bne_true);

    and take_blt(blt_taken, alu_bne_true, !alu_lt, DX_blt_true);

    assign XM_LATCH_IR_in = (DX_bne_true || DX_blt_true) ? 32'b0 : DX_LATCH_IR_out;



    
// **** MULTIPLIER/DIVIDER ****
    // output from multdiv sent to MUX after MW latch that is selected to be sent to the register file
    // MULT ALU OP: 00110
    // DIV ALU OP: 00111    

    assign op_mult = (ALUop_dx == 5'b00110) && (opcode_dx == 5'b00000);
    assign op_div = (ALUop_dx == 5'b00111) && (opcode_dx == 5'b00000);

    // dff to store op_mult and op_div
    dffe_ref STABLE_MULTSIGNAL(.q(op_mult_stable), .d(op_mult), .clk(clock), .en(op_mult), .clr(multdiv_resultRDY));
    dffe_ref STABLE_DIVSIGNAL(.q(op_div_stable), .d(op_div), .clk(clock), .en(op_div), .clr(multdiv_resultRDY));



    // multdiv multiplyDivide(.data_operandA(DX_LATCH_A), .data_operandB(DX_LATCH_B), .ctrl_MULT(op_mult), .ctrl_DIV(op_div), 
    // .clock(clock), .data_result(multdiv_result), .data_exception(multdiv_exception), .data_resultRDY(multdiv_resultRDY), .multRDY(multReady), .divRDY(divReady));
    // multdiv multiplyDivide(.data_operandA(ALU_opA), .data_operandB(rt_ALU_opB), .ctrl_MULT(op_mult), .ctrl_DIV(op_div), 
    // .clock(clock), .data_result(multdiv_result), .data_exception(multdiv_exception), .data_resultRDY(multdiv_resultRDY), .multRDY(multReady), .divRDY(divReady));
    multdiv multiplyDivide(.data_operandA(ALU_opA), .data_operandB(rt_ALU_opB), .ctrl_MULT(op_mult), .ctrl_DIV(op_div), 
    .clock(clock), .data_result(multdiv_result), .data_exception(multdiv_exception), .data_resultRDY(multdiv_resultRDY), .multRDY(multReady), .divRDY(divReady));

    assign multdiv_stall = multdiv_resultRDY ? 1'b0 : DX_LATCH_IR_out[31:27] == 5'b00000 && ((DX_LATCH_IR_out[6:2] == 5'b00110) || (DX_LATCH_IR_out[6:2] == 5'b00110));

// multdiv_stall = data_resultRDY ? 1'b0 : DX_IR_out[31:27] == 5'b00000 && (DX_IR_out[6:2] == 5'b00110) || (DX_IR_out[6:2] == 5'b00110)
    

// **** P/W LATCH ****
    // Stores multdiv result and IR
    regFallingEdge PW_LATCH_MULTDIVRESULT(.data(multdiv_result), .clk(clock), .reset(reset), .enable(multdiv_resultRDY), .out(multdiv_result_pw));
    regFallingEdge PW_LATCH_IR(.data(DX_LATCH_IR_out), .clk(clock), .reset(reset), .enable(|(DX_LATCH_IR_out)), .out(PW_LATCH_IR_out));
    // enable condition is that a NOP hasn't been inserted, so if IR is all 0 the IR cannot be updated
    dffe_fallEdge PW_LATCH_MULT(.q(multdiv_resultRDY_pw), .d(multdiv_resultRDY), .clk(clock), .en(1'b1), .clr(reset));
    dffe_fallEdge PW_LATCH_EXC(.q(pw_multdiv_exception), .d(multdiv_exception), .clk(clock), .en(1'b1), .clr(reset));
    dffe_fallEdge PW_LATCH_MULTRDY(.q(pw_multReady), .d(multReady), .clk(clock), .en(1'b1), .clr(reset));
    dffe_fallEdge PW_LATCH_DIVRDY(.q(pw_divReady), .d(divReady), .clk(clock), .en(1'b1), .clr(reset));

    assign multdiv_resultRDY_pw_check = multdiv_resultRDY_pw && (PW_LATCH_IR_out[31:27] == 5'b00000 && (PW_LATCH_IR_out[6:2] == 5'b00110 || PW_LATCH_IR_out[6:2] == 5'b00111));
    assign pw_writereg = (pw_multdiv_exception && multdiv_resultRDY_pw) ? 5'b11110 : PW_LATCH_IR_out[26:22];
    assign pw_writedata = (pw_multdiv_exception && multdiv_resultRDY_pw && pw_multReady) ? {{(29){1'b0}}, {1'b1}, {(2){1'b0}}} : ((pw_multdiv_exception && multdiv_resultRDY_pw && pw_divReady) ? {{(29){1'b0}}, {1'b1}, {1'b0}, {1'b1}} : multdiv_result_pw);



// **** X/M LATCH ****
    regFallingEdge XM_LATCH_ALUout(.data(ALU_out), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(XM_LATCH_O));
    regFallingEdge XM_LATCH_opB(.data(DX_LATCH_B), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(XM_LATCH_B));
    regFallingEdge XM_LATCH_ir(.data(XM_LATCH_IR_in), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(XM_LATCH_IR));
    dffe_fallEdge XM_LATCH_aluEXC(.q(alu_overflow_xm), .d(alu_overflow), .clk(clock), .en(!multdiv_stall), .clr(reset));
    regFallingEdge XM_LATCH_PC(.data(DX_LATCH_PC_out), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(XM_LATCH_PC_out));


    // stores alu output, operand b, instruction (not pc here)


    
// **** DMEM ****

    // DMEM data bypassing: select bypassed input if MW rd is equal to XM rd and sw insn
    assign xm_sw_true = (XM_LATCH_IR[31:27] == 5'b00111);
    assign xm_lw_true = (XM_LATCH_IR[31:27] == 5'b01000);
    // rd is written to in alu ops, addi, lw
    assign xm_rdWritten_mw_alu = (MW_LATCH_IR[31:27] == 5'b00000) || (MW_LATCH_IR[31:27] == 5'b00101);
    assign xm_rdWritten_mw_lw = (MW_LATCH_IR[31:27] == 5'b01000);
    assign xm_sw_bypass = (XM_LATCH_IR[26:22] == mw_writereg) && (XM_LATCH_IR[26:22] != 5'b0); 

    assign DMEM_data = (xm_sw_true && xm_sw_bypass && xm_rdWritten_mw_alu) ? mw_alu_result_exc : (xm_sw_true && xm_sw_bypass && xm_rdWritten_mw_lw) ? lw_mem_out : XM_LATCH_B;

    // TODO: also need to check opcode of next insn? in case bits happen to match???? or deal with jump insn separately?

    // OUTPUTS/INPUTS TO ELEMENTS IN WRAPPER MODULE
    assign address_dmem = XM_LATCH_O;
    assign data = DMEM_data;
    assign wren = (XM_LATCH_IR[31:27] == 5'b00111); // TODO: update to xm_sw_true
    assign DMEM_out = q_dmem;


    // ALL REDUNDANT, NEED FOR BYPASSING, RECALCUALTE IN MW
    // TODO: latch these values instead of others currently in mw latch

    assign xm_not_nop = (XM_LATCH_IR != 32'b0);
    assign xm_add_op = (XM_LATCH_IR[6:2] == 5'b00000)&& (XM_LATCH_IR[31:27] == 5'b00000) && xm_not_nop;
    assign xm_sub_op = (XM_LATCH_IR[6:2]  == 5'b00001)&& (XM_LATCH_IR[31:27] == 5'b00000);
    assign xm_addi_op = (XM_LATCH_IR[31:27] == 5'b00101);


    assign xm_writereg = (alu_overflow_xm && (xm_add_op || xm_sub_op || xm_addi_op)) ? 5'b11110 : XM_LATCH_IR[26:22]; 
    assign xm_alu_result_exc = (alu_overflow_xm && xm_sub_op) ? {{(30){1'b0}}, {(2){1'b1}}} : ( (alu_overflow_xm && xm_addi_op) ? {{(30){1'b0}}, {1'b1}, {1'b0}} : ((alu_overflow_xm && xm_add_op ) ? {{(31){1'b0}}, {1'b1}} : XM_LATCH_O));
    //assign xm_alu_result_exc = XM_LATCH_O;


// **** M/W LATCH ****
    regFallingEdge MW_LATCH_DMEMout(.data(DMEM_out), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(lw_mem_out));
	regFallingEdge MW_LATCH_ALUout(.data(XM_LATCH_O), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(regfile_input));
    regFallingEdge MW_LATCH_ir(.data(XM_LATCH_IR), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(MW_LATCH_IR));
    dffe_fallEdge MW_LATCH_aluEXC(.q(alu_overflow_mw), .d(alu_overflow_xm), .clk(clock), .en(!multdiv_stall), .clr(reset));
    dffe_fallEdge MW_LATCH_mdEXC(.q(mw_multdiv_exception), .d(multdiv_resultRDY_pw), .clk(clock), .en(!multdiv_stall), .clr(reset));
    regFallingEdge MW_LATCH_PC(.data(XM_LATCH_PC_out), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(MW_LATCH_PC_out));
    regFallingEdge MW_LATCH_MULTDIV(.data(pw_writedata), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(multdiv_result_mw));
    regFallingEdge_5bit MW_LATCH_MULTDIVREG(.data(pw_writereg), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(mw_multdiv_writereg));
    dffe_fallEdge MW_LATCH_MULTDIVREADY(.q(multdiv_resultRDY_mw), .d(multdiv_resultRDY_pw), .clk(clock), .en(!multdiv_stall), .clr(reset));



    assign mw_not_nop = (MW_LATCH_IR != 32'b0);
    assign x_add_op = (MW_LATCH_IR[6:2] == 5'b00000)&& (MW_LATCH_IR[31:27] == 5'b00000) && mw_not_nop;
    assign x_sub_op = (MW_LATCH_IR[6:2]  == 5'b00001)&& (MW_LATCH_IR[31:27] == 5'b00000);
    assign x_addi_op = (MW_LATCH_IR[31:27] == 5'b00101);



    assign mw_sw_true = (MW_LATCH_IR[31:27] == 5'b00111);
    assign mw_lw_true = (MW_LATCH_IR[31:27] == 5'b01000);
    assign mw_setx_true = (MW_LATCH_IR[31:27] == 5'b10101);


    assign mw_writereg = (alu_overflow_mw && (x_add_op || x_sub_op || x_addi_op)) ? 5'b11110 : MW_LATCH_IR[26:22]; 
    assign mw_alu_result_exc = (alu_overflow_mw && x_sub_op) ? {{(30){1'b0}}, {(2){1'b1}}} : ( (alu_overflow_mw && x_addi_op) ? {{(30){1'b0}}, {1'b1}, {1'b0}} : ((alu_overflow_mw && x_add_op ) ? {{(31){1'b0}}, {1'b1}} : regfile_input));

    assign mw_target_extended = {{5{MW_LATCH_IR[26]}}, MW_LATCH_IR[26:0]};

    assign mw_jal_true = (MW_LATCH_IR[31:27] == 5'b00011);
    assign mw_jr_true = (MW_LATCH_IR[31:27] == 5'b00100);



// ** HAZARD DETECTION **

    // conditions to check 
    // f/d latch source register 1 == d/x latch destination register
    // f/d latch source register 2 == d/x latch destination register
    // f/d latch source register 1 == x/m latch destination register
    // f/d latch source register 2 == x/m latch destination register
    // this is all one big or statement where a 1 indicates insert a nop

// *** BYPASS LOGIC ***

    // write nop into processor then recheck condition (check same one every time)


endmodule


// /**
//  * READ THIS DESCRIPTION!
//  *
//  * This is your processor module that will contain the bulk of your code submission. You are to implement
//  * a 5-stage pipelined processor in this module, accounting for hazards and implementing bypasses as
//  * necessary.
//  *
//  * Ultimately, your processor will be tested by a master skeleton, so the
//  * testbench can see which controls signal you active when. Therefore, there needs to be a way to
//  * "inject" imem, dmem, and regfile interfaces from some external controller module. The skeleton
//  * file, Wrapper.v, acts as a small wrapper around your processor for this purpose. Refer to Wrapper.v
//  * for more details.
//  *
//  * As a result, this module will NOT contain the RegFile nor the memory modules. Study the inputs 
//  * very carefully - the RegFile-related I/Os are merely signals to be sent to the RegFile instantiated
//  * in your Wrapper module. This is the same for your memory elements. 
//  *
//  *
//  */

// // 32 bit processor!

// // *** QUESTIONS ***

// // *** TO DO ***
//     // Ensure that 0 register can only be set to 0, and that it will always stay at 0
//     // don't increment PC when NOP inserted
//     // check insn for bypassing actually writes to rd register (not sw, bne, blt)


// module processor(
//     // Control signals
//     clock,                          // I: The master clock
//     reset,                          // I: A reset signal

//     // Imem
//     address_imem,                   // O: The address of the data to get from imem
//     q_imem,                         // I: The data from imem

//     // Dmem
//     address_dmem,                   // O: The address of the data to get or put from/to dmem
//     data,                           // O: The data to write to dmem
//     wren,                           // O: Write enable for dmem
//     q_dmem,                         // I: The data from dmem

//     // Regfile
//     ctrl_writeEnable,               // O: Write enable for RegFile
//     ctrl_writeReg,                  // O: Register to write to in RegFile
//     ctrl_readRegA,                  // O: Register to read from port A of RegFile
//     ctrl_readRegB,                  // O: Register to read from port B of RegFile
//     data_writeReg,                  // O: Data to write to for RegFile
//     data_readRegA,                  // I: Data from port A of RegFile
//     data_readRegB                   // I: Data from port B of RegFile
	 
// 	);

// 	// Control signals
// 	input clock, reset;
	
// 	// Imem
//     output [31:0] address_imem;
// 	input [31:0] q_imem;

// 	// Dmem
// 	output [31:0] address_dmem, data;
// 	output wren;
// 	input [31:0] q_dmem;

// 	// Regfile
// 	output ctrl_writeEnable;
// 	output [4:0] ctrl_writeReg, ctrl_readRegA, ctrl_readRegB;
// 	output [31:0] data_writeReg;
// 	input [31:0] data_readRegA, data_readRegB;

//     // wires to connect components
//     wire [31:0] reg_outA, reg_outB;
//     wire [31:0] DX_LATCH_IN_A, DX_LATCH_IN_B, DX_LATCH_A, DX_LATCH_B, DX_LATCH_IR_out, DX_IR_in;
//     wire [31:0] ALU_out, DMEM_out, regfile_input, ALU_opB;
//     wire [31:0] XM_LATCH_O, XM_LATCH_B, XM_LATCH_IR, MW_LATCH_IR, XM_LATCH_IR_in, XM_LATCH_PC_out;

//     // PC CONTROL WIRES
//     wire [31:0] pc_reg_in, pc_reg_out, imem_out, pc_increment_amt, DX_LATCH_PC_out, pc_input; // PC wires
//     wire pc_increment_last, x;

//     // FD LATCH WIRES
//     wire [31:0] pc_FDlatch_in, pc_FDlatch_out, IR_FD, FD_IR_IN, IR_FD_1;

//     // PW LATCH WIRES
//     wire [31:0] PW_LATCH_IR_out;

//     //control wires
//     wire [4:0] opcode, rd, rs, rt, shamt, ALUop;
//     wire [31:0] imm;
//     wire [26:0] target;

//     // control after dx
//     wire [4:0] opcode_dx, rd_dx, rs_dx, rt_dx, shamt_dx, ALUop_dx, aluCode;
//     wire [31:0] imm_dx;

//     wire addi_true;

//     // MULTDIV wires
//     wire op_mult, op_div, multdiv_resultRDY, multdiv_exception, mult_op, div_op, multdiv_resultRDY_pw;
//     wire [31:0] multdiv_result, multdiv_result_pw, multdiv_result_exc_pw;
//     wire [5:0] mult_counter, div_counter;

//     // sw and lw wires
//     wire [31:0] lw_mem_out;
//     wire decode_sw_true, sw_true, lw_true, xm_lw_true, mw_sw_true, mw_lw_true;

//     // j insn wires
//     wire [31:0] jump_addr, jr_addr, jal_addr;
//     wire DX_jump_true, DX_jr_true, DX_jal_true, decode_jr_true, decode_jump_true, decode_jal_true;

//     // branch insn wires
//     wire [31:0] branch_addr;
//     wire decode_bne_true, decode_blt_true, DX_bne_true, DX_blt_true, alu_bne_true, bne_taken, blt_taken, alu_lt, branch_taken;

//     // bex and setx wires
//     wire [31:0] mw_target_extended, mw_alu_result_exc, pw_writedata, rstatus_exceptions, MW_LATCH_PC_out;
//     wire mw_setx_true, DX_bex_true, decode_bex_true, alu_overflow, zero_opcode, x_add_op, x_sub_op, x_mul_op, x_div_op, alu_overflow_xm, alu_overflow_mw, pw_multdiv_exception, mw_multdiv_exception;
//     wire x_addi_op, multReady, divReady, pw_multReady, pw_divReady, mw_not_nop, mw_jal_true, DX_lw_true;
//     wire [4:0] mw_writereg, mw_exc_writereg, pw_writereg, xm_writereg;

//     // bypassing wires
//     wire [31:0] rt_ALU_opB, ALU_opA, DMEM_data, xm_alu_result_exc;
//     wire rs1_bypassOpTrue, rs2_bypassOpTrue, rs1_eq_xmOut, rs1_eq_mwOut, rs2_eq_xmOut, rs2_eq_mwOut, dx_rdWritten_xm, dx_rdWritten_mw, xm_sw_true, xm_sw_bypass;
//     wire xm_not_nop, xm_add_op, xm_sub_op, xm_addi_op, xm_rdWritten_mw_alu, xm_rdWritten_mw_lw, rs1_notZero, rs2_notZero;
//     wire dx_rd_eq_xmOut, dx_rd_eq_mwOut, rd_notZero, mw_jr_true;

//     // stall wires
//     wire fd_lw_dependency, fd_lw_stall, fd_lw_stall_last, op_mult_stable, op_div_stable, mult_op_fd, div_op_fd, multdiv_stall, multdiv_resultRDY_mw;
//     wire [31:0] xm_bypass_input, mw_bypass_input, multdiv_result_mw;
//     wire [4:0] ctrl_readRegA_dx, ctrl_readRegB_dx, mw_multdiv_writereg;
//     wire xm_mul_op, xm_div_op, mw_mul_op, mw_div_op;

// // **** PC **** 
//     // register to store pc, 32 bits, this can be normal one that is rising edge triggered
//     // latch after this stage stores pc
//     // use adder to increment pc (delay doesn't matter because of pipelining)
//     // register to store pc


//     addCalc newAddr(.A(DX_LATCH_PC_out), .B(imm_dx), .S(branch_addr), .isNotEqual(), .isLessThan(), .overflow(), .subTrue(1'b0));
//     // mux to select between jump insn output and incremented pc
//     assign pc_input = (bne_taken || blt_taken) ? branch_addr : ((DX_jump_true || DX_jal_true || DX_bex_true) ? jump_addr : (DX_jr_true ? jr_addr : pc_reg_in));

//     //regOne pc_register(.data(pc_input), .clk(~clock), .reset(reset), .enable((1'b1)), .out(pc_reg_out)); 
//     regOne pc_register(.data(pc_input), .clk(~clock), .reset(reset), .enable(!multdiv_stall), .out(pc_reg_out)); // yyy
//     // increment register by 1
//     addCalc INCREMENT_PC(.A(pc_reg_out), .B(pc_increment_amt), .S(pc_reg_in), .isNotEqual(), .isLessThan(), .overflow(), .subTrue(1'b0));

//     // if multiply instruction, don't increment pc until after the operation is complete, don't increment for 16 cycles?
//     // change input to adder from 1 to 0

//     // imem fetch: only get new pc if mult or div is not true


// // **** IMEM **** 
//     // this is ROM, OUTPUTS/INPUTS TO ELEMENTS IN WRAPPER MODULE
//     assign address_imem = pc_reg_out;
//     assign imem_out = q_imem;

//     assign mult_op = (imem_out[6:2] == 5'b00110) && (imem_out[31:27] == 5'b00000);
//     assign div_op = (imem_out[6:2] == 5'b00111) && (imem_out[31:27] == 5'b00000);
//     // assign mult_op = op_mult_stable;
//     // assign div_op = op_div_stable;

//     // reset condition for counter: mult op is the current insn and the pc reg was previously incremented (first cycle of mult op) or counter at val
//     //counter multcounter(mult_counter, clock, ((mult_op || div_op) && !(pc_reg_in == pc_reg_out)) || multdiv_resultRDY_pw);    
//     //counter multcounter(mult_counter, clock, ((mult_op || div_op) && !(pc_reg_in == pc_reg_out)) || multdiv_resultRDY_pw);    


//     // increment 0 if mult_op and mult_counter is not 16, same for div but 32

//     // increment conditions: mult op is high, the result is not ready and last increment amount was 0
//     //assign pc_increment_amt =  fd_lw_stall || (!multdiv_resultRDY_pw && ((mult_op || div_op) || (!fd_lw_stall_last && !pc_increment_last && (mult_counter != 6'b000000)))) ? 32'b0 : {{31'b0},{1'b1}};
//     //assign pc_increment_amt =  fd_lw_stall || (!multdiv_resultRDY_pw && ((mult_op || div_op) || (!fd_lw_stall_last && !pc_increment_last && (mult_counter != 6'b000000)))) ? 32'b0 : {{31'b0},{1'b1}};
//     assign pc_increment_amt = fd_lw_stall ? 32'b0 : {{31'b0},{1'b1}}; //yyy

//     // dffe_ref (q, d, clk, en, clr);
//     dffe_ref pc_increment_prev(pc_increment_last, pc_increment_amt[0], clock, 1'b1, reset);
//     dffe_ref fd_lw_stall_prev(fd_lw_stall_last, fd_lw_stall, clock, 1'b1, reset);


//     // FOR FD LATCH IR IN: select imem_out if pc != prev pc, else select all 0s
//     //assign FD_IR_IN = (((pc_reg_in == pc_reg_out) && (mult_counter != 6'b000000)) && !fd_lw_stall) || branch_taken ? 32'b0 : imem_out;
//      assign FD_IR_IN = branch_taken ? 32'b0 : imem_out;


// // **** F/D LATCH ****
//     regFallingEdge FD_LATCH_PC(.data(pc_reg_in), .clk(clock), .reset(reset), .enable(!fd_lw_stall && !multdiv_stall), .out(pc_FDlatch_out)); //yyy enable
//     regFallingEdge FD_LATCH_IR(.data(FD_IR_IN), .clk(clock), .reset(reset), .enable(!fd_lw_stall && !multdiv_stall), .out(IR_FD_1)); 
//     dffe_ref FD_LATCH_MULTOP(.q(mult_op_fd), .d(mult_op), .clk(~clock), .en(1'b1), .clr(reset));
//     dffe_ref FD_LATCH_DIVOP(.q(div_op_fd), .d(div_op), .clk(~clock), .en(1'b1), .clr(reset));


//     assign branch_taken = (DX_jump_true || DX_jal_true  || DX_jr_true || bne_taken || blt_taken || DX_bex_true);
//     assign IR_FD =  branch_taken ? 32'b0 : IR_FD_1;

//     control INSN_CONTROL(IR_FD, opcode, rd, rs, rt, shamt, ALUop, imm, target);

//     and decode_sw_insn(decode_sw_true, !opcode[4], !opcode[3], opcode[2], opcode[1], opcode[0]);
//     and decode_jump_insn(decode_jump_true, !opcode[4], !opcode[3], !opcode[2], !opcode[1], opcode[0]);
//     and decode_jr_insn(decode_jr_true, !opcode[4], !opcode[3], opcode[2], !opcode[1], !opcode[0]);
//     and decode_jal_insn(decode_jal_true, !opcode[4], !opcode[3], !opcode[2], opcode[1], opcode[0]);
//     and decode_bne_insn(decode_bne_true, !opcode[4], !opcode[3], !opcode[2], opcode[1], !opcode[0]);
//     and decode_blt_insn(decode_blt_true, !opcode[4], !opcode[3], opcode[2], opcode[1], !opcode[0]);
//     and decode_bex_insn(decode_bex_true, opcode[4], !opcode[3], opcode[2], opcode[1], !opcode[0]);


// // **** STALL LOGIC ****
    
//     // wires: fd_lw_true, fd_lw_dependency, fd_lw_stall

//     assign fd_lw_dependency = (IR_FD[21:17] == DX_LATCH_IR_out[26:22]) || ((IR_FD[16:12] == DX_LATCH_IR_out[26:22]) && (IR_FD != 5'b00111));
//     // stall when lw insn, reading from write reg, and next insn is not a sw
//     assign fd_lw_stall = DX_lw_true && fd_lw_dependency;



// // **** REGISTER FILE ****
//     // DONT HAVE AN INSTANCE OF THE REGFILE HERE!!
//     // SEND TO OUTPUTS AND INPUTS IN WRAPPER MODULE

//     // select correct input bit to register, pw vs mw latch output
//     //assign ctrl_writeEnable = !mw_sw_true && (mw_jal_true || ((MW_LATCH_IR[31:27] == 5'b00000) && ((MW_LATCH_IR[6:4] == 3'b000) || (MW_LATCH_IR[6:4] == 4'b0010))) || (MW_LATCH_IR[31:27] == 5'b00101) || ((PW_LATCH_IR_out[6:3] == 4'b0011) && multdiv_resultRDY_pw)) ? 1'b1 : 1'b0; //&& (MW_LATCH_IR != 32'b0)
//     //assign ctrl_writeEnable = !mw_sw_true && !mw_jr_true && (mw_jal_true || (MW_LATCH_IR[6:4] == 3'b000) || (MW_LATCH_IR[6:3] == 4'b0010) || (MW_LATCH_IR[31:27] == 5'b00101) || ((MW_LATCH_IR[6:3] == 4'b0011) && multdiv_resultRDY_mw)) ? 1'b1 : 1'b0; //&& (MW_LATCH_IR != 32'b0)
//     assign ctrl_writeEnable = !mw_sw_true && !mw_jr_true && (mw_jal_true || (MW_LATCH_IR[6:4] == 3'b000) || (MW_LATCH_IR[6:3] == 4'b0010) || (MW_LATCH_IR[31:27] == 5'b00101) || ((PW_LATCH_IR_out[6:3] == 4'b0011) && multdiv_resultRDY_pw)) ? 1'b1 : 1'b0; //&& (MW_LATCH_IR != 32'b0)
//     // enabled with NOP, but writes to zero register so not an issue
//     // assign ctrl_writeReg = mw_setx_true ? 5'b11110 : (mw_jal_true ? 5'b11111 : (mw_lw_true ? MW_LATCH_IR[26:22] : ((multdiv_resultRDY_pw && !pc_increment_last) ? pw_writereg : mw_writereg))); //SIMPLIFY THIS
//     assign ctrl_writeReg = mw_setx_true ? 5'b11110 : (mw_jal_true ? 5'b11111 : (mw_lw_true ? MW_LATCH_IR[26:22] : ((multdiv_resultRDY_pw) ? pw_writereg : mw_writereg))); //YYY
//     assign ctrl_readRegA = decode_bex_true ? 5'b11110 : rs;
//     assign ctrl_readRegB = (decode_sw_true || decode_jr_true || decode_bne_true || decode_blt_true) ? rd : rt; // get jr address from reg B read
//     //assign data_writeReg = mw_setx_true ? mw_target_extended : (mw_jal_true ? MW_LATCH_PC_out : (mw_lw_true ? lw_mem_out : ((multdiv_resultRDY_pw && !pc_increment_last) ? pw_writedata : mw_alu_result_exc)));
//     assign data_writeReg = mw_setx_true ? mw_target_extended : (mw_jal_true ? MW_LATCH_PC_out : (mw_lw_true ? lw_mem_out : ((multdiv_resultRDY_pw) ? pw_writedata : mw_alu_result_exc)));
//     assign reg_outA = data_readRegA;
//     assign reg_outB = data_readRegB; // read from rd if sw





// // **** D/X LATCH ****

//     // instantiate counter
//     // regFallingEdge DX_LATCH_opA(.data(reg_outA), .clk(clock), .reset(reset), .enable(pc_increment_last || (mult_counter == 6'b000001)), .out(DX_LATCH_A));
//     // regFallingEdge DX_LATCH_opB(.data(reg_outB), .clk(clock), .reset(reset), .enable(pc_increment_last || (mult_counter == 6'b000001)), .out(DX_LATCH_B));
//     regFallingEdge DX_LATCH_opA(.data(reg_outA), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(DX_LATCH_A)); // yyy enable
//     regFallingEdge DX_LATCH_opB(.data(reg_outB), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(DX_LATCH_B));

//     assign DX_IR_in = (DX_jump_true || DX_jal_true  || DX_jr_true || bne_taken || blt_taken || DX_bex_true || fd_lw_stall) ? 32'b0 : IR_FD;
//     regFallingEdge DX_LATCH_PC(.data(pc_FDlatch_out), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(DX_LATCH_PC_out));
//     regFallingEdge DX_LATCH_IR(.data(DX_IR_in), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(DX_LATCH_IR_out));
//     regFallingEdge_5bit DX_LATCH_opcode(.data(opcode), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(opcode_dx));
//     regFallingEdge_5bit DX_LATCH_rd(.data(rd), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(rd_dx));
//     regFallingEdge_5bit DX_LATCH_rs(.data(rs), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(rs_dx));
//     regFallingEdge_5bit DX_LATCH_rt(.data(rt), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(rt_dx));
//     regFallingEdge_5bit DX_LATCH_shamt(.data(shamt), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(shamt_dx));
//     regFallingEdge_5bit DX_LATCH_ALUop(.data(ALUop), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(ALUop_dx));
//     regFallingEdge DX_LATCH_imm(.data(imm), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(imm_dx));
//     regFallingEdge_5bit DX_LATCH_readRegA(.data(ctrl_readRegA), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(ctrl_readRegA_dx));
//     regFallingEdge_5bit DX_LATCH_readRegB(.data(ctrl_readRegB), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(ctrl_readRegB_dx));

//     // stores operand A, operand B, PC, instruction, and control

//     assign DX_jump_true = (DX_LATCH_IR_out[31:27] == 5'b00001);
//     assign DX_jr_true = (DX_LATCH_IR_out[31:27] == 5'b00100);
//     assign DX_jal_true = (DX_LATCH_IR_out[31:27] == 5'b00011);
//     assign DX_bne_true = (DX_LATCH_IR_out[31:27] == 5'b00010);
//     assign DX_blt_true = (DX_LATCH_IR_out[31:27] == 5'b00110);
//     assign DX_bex_true = (DX_LATCH_IR_out[31:27] == 5'b10110) && (DX_LATCH_A != 32'b0);
//     assign DX_lw_true = (DX_LATCH_IR_out[31:27] == 5'b01000);

//     // address to jump to (see jr addr in bypass logic section)
//     assign jump_addr = {{6{DX_LATCH_IR_out[26]}}, DX_LATCH_IR_out[25:0]};
//     //assign jr_addr = DX_LATCH_B;
//     //assign jr_addr = rt_ALU_opB;

//     // if following insn or two insns write to rd, select different jr input



// // **** BYPASS LOGIC ALU/MULTDIV INPUT ****

//     // implement for read after write dependencies

//     // conditions to check 
//     // d/x latch source register 1 == x/m latch destination register
//     // d/x latch source register 2 == x/m latch destination register
//     // f/d latch source register 1 == m/w latch destination register
//     // f/d latch source register 2 == m/w latch destination register
//     // this is all one big or statement where a 1 indicates insert a nop

//     // NOTE: ONLY IMPLEMENTED BYPASS LOGIC FOR NORMAL ALU OPS AND ADDI RN ADD OTHERS LATER 

//     // D/X latch source 1: implement for opcode 00000, 00101 addi  
//         // module mux_4(out, select, in0, in1, in2, in3);
//         // mux select 01: d/x latch source reg 1 == x/m latch dest reg
//         // mux select 10: d/x latch source reg 1 == m/w latch dest reg
//         // mux select 00: else
//         // mux select 11: pick xm output bc that will have correct result

//     // rd is written to in alu ops, addi, lw
//     assign dx_rdWritten_xm = (XM_LATCH_IR[31:27] == 5'b00000) || (XM_LATCH_IR[31:27] == 5'b00101) || (XM_LATCH_IR[31:27] == 5'b01000);
//     assign dx_rdWritten_mw = (MW_LATCH_IR[31:27] == 5'b00000) || (MW_LATCH_IR[31:27] == 5'b00101) || (MW_LATCH_IR[31:27] == 5'b01000);

//     assign rs1_notZero = (ctrl_readRegA_dx != 5'b00000);
//     assign rs2_notZero = (ctrl_readRegB_dx != 5'b00000);
//     assign rd_notZero = (rd_dx != 5'b00000);

//     // if lw true, select output of dmem instead of output of alu
//     // this applies to both xm and mw stage
//     assign xm_bypass_input = xm_lw_true ? DMEM_out : xm_alu_result_exc ;
//     // add multdiv result to xm bypass input
//     //assign xm_bypass_input = (xm_mul_op || xm_div_op) ? multdiv_result_pw : (xm_lw_true ? DMEM_out : xm_alu_result_exc );

//     assign mw_bypass_input = mw_lw_true ? lw_mem_out : mw_alu_result_exc ;


//     // need to bypass jr when rd written in next two insns DX_jr_true
//     // assign dx_rd_eq_xmOut = (rd_dx == xm_writereg) && DX_jr_true && rd_notZero;
//     // assign dx_rd_eq_mwOut = (rd_dx == mw_writereg) && DX_jr_true && rd_notZero;
//     assign dx_rd_eq_xmOut = (ctrl_readRegB_dx == xm_writereg) && DX_jr_true && rs2_notZero;
//     assign dx_rd_eq_mwOut = (ctrl_readRegB_dx == mw_writereg) && DX_jr_true && rs2_notZero;

//     mux_4 bypass_select_jr(jr_addr, {{dx_rd_eq_xmOut}, {dx_rd_eq_mwOut}}, DX_LATCH_B, mw_bypass_input, xm_bypass_input, xm_bypass_input); 



//     assign rs1_bypassOpTrue = ((opcode_dx == 5'b00000) || (opcode_dx == 5'b00101) || (opcode_dx == 5'b00111) || (opcode_dx == 5'b01000)|| (opcode_dx == 5'b00110)|| (opcode_dx == 5'b00010));
//     // assign rs1_eq_xmOut = rs1_bypassOpTrue && (rs_dx == xm_writereg) && dx_rdWritten_xm && rs1_notZero;
//     // assign rs1_eq_mwOut = rs1_bypassOpTrue && (rs_dx == mw_writereg) && dx_rdWritten_mw && rs1_notZero; // below change reg to check
//     assign rs1_eq_xmOut = rs1_bypassOpTrue && (ctrl_readRegA_dx == xm_writereg) && dx_rdWritten_xm && rs1_notZero;
//     assign rs1_eq_mwOut = rs1_bypassOpTrue && (ctrl_readRegA_dx == mw_writereg) && dx_rdWritten_mw && rs1_notZero;

//     // mux_4 bypass_select_rs1(ALU_opA, {{rs1_eq_xmOut}, {rs1_eq_mwOut}}, DX_LATCH_A, mw_alu_result_exc, xm_alu_result_exc, xm_alu_result_exc); 
//     mux_4 bypass_select_rs1(ALU_opA, {{rs1_eq_xmOut}, {rs1_eq_mwOut}}, DX_LATCH_A, mw_bypass_input, xm_bypass_input, xm_bypass_input); 


    
//     // D/X latch source 2: implement for opcode 00000
//         // mux select 01: d/x latch source reg 2 == x/m latch dest reg
//         // mux select 10: d/x latch source reg 2 == m/w latch dest reg
//         // mux select 00: else

//     assign rs2_bypassOpTrue = (opcode_dx == 5'b00000 && ALUop_dx!=5'b00100 && ALUop_dx!=5'b00101) || (opcode_dx == 5'b00110)|| (opcode_dx == 5'b00010);
//     // assign rs2_eq_xmOut = rs2_bypassOpTrue && (rt_dx == xm_writereg) && rs2_notZero;
//     // assign rs2_eq_mwOut = rs2_bypassOpTrue && (rt_dx == mw_writereg) && rs2_notZero;
//     assign rs2_eq_xmOut = rs2_bypassOpTrue && (ctrl_readRegB_dx == xm_writereg) && rs2_notZero;
//     assign rs2_eq_mwOut = rs2_bypassOpTrue && (ctrl_readRegB_dx == mw_writereg) && rs2_notZero;
//     // mux_4 bypass_select_rs2(rt_ALU_opB, {{rs2_eq_xmOut}, {rs2_eq_mwOut}}, DX_LATCH_B, mw_alu_result_exc, xm_alu_result_exc, xm_alu_result_exc);
//     mux_4 bypass_select_rs2(rt_ALU_opB, {{rs2_eq_xmOut}, {rs2_eq_mwOut}}, DX_LATCH_B, mw_bypass_input, xm_bypass_input, xm_bypass_input);





// // **** ALU ****


//     // and opcode 00101
//     and addi_insn(addi_true, !opcode_dx[4], !opcode_dx[3], opcode_dx[2], !opcode_dx[1], opcode_dx[0]);
//     and sw_insn(sw_true, !opcode_dx[4], !opcode_dx[3], opcode_dx[2], opcode_dx[1], opcode_dx[0]);
//     and lw_insn(lw_true, !opcode_dx[4], opcode_dx[3], !opcode_dx[2], !opcode_dx[1], !opcode_dx[0]);

//     // select immediate if addi or sw
//     assign ALU_opB = (addi_true || sw_true || lw_true) ? imm_dx : rt_ALU_opB;
//     assign aluCode = DX_blt_true ? 5'b00001 : ((addi_true || sw_true || lw_true) ? {5'b0} : ALUop_dx);


//     alu ALU(.data_operandA(ALU_opA), .data_operandB(ALU_opB), .ctrl_ALUopcode(aluCode), 
//     .ctrl_shiftamt(shamt_dx), .data_result(ALU_out), .isNotEqual(alu_bne_true), .isLessThan(alu_lt), .overflow(alu_overflow));
//     // operand a from reg read port 1
//     // operand b from reg read port 2

//     // check for bne condition
//     and take_bne(bne_taken, alu_bne_true, DX_bne_true);

//     and take_blt(blt_taken, alu_bne_true, !alu_lt, DX_blt_true);

//     assign XM_LATCH_IR_in = (DX_bne_true || DX_blt_true) ? 32'b0 : DX_LATCH_IR_out;



    
// // **** MULTIPLIER/DIVIDER ****
//     // output from multdiv sent to MUX after MW latch that is selected to be sent to the register file
//     // MULT ALU OP: 00110
//     // DIV ALU OP: 00111    

//     assign op_mult = (ALUop_dx == 5'b00110) && (opcode_dx == 5'b00000);
//     assign op_div = (ALUop_dx == 5'b00111) && (opcode_dx == 5'b00000);

//     // dff to store op_mult and op_div
//     dffe_ref STABLE_MULTSIGNAL(.q(op_mult_stable), .d(op_mult), .clk(clock), .en(op_mult), .clr(multdiv_resultRDY));
//     dffe_ref STABLE_DIVSIGNAL(.q(op_div_stable), .d(op_div), .clk(clock), .en(op_div), .clr(multdiv_resultRDY));



//     // multdiv multiplyDivide(.data_operandA(DX_LATCH_A), .data_operandB(DX_LATCH_B), .ctrl_MULT(op_mult), .ctrl_DIV(op_div), 
//     // .clock(clock), .data_result(multdiv_result), .data_exception(multdiv_exception), .data_resultRDY(multdiv_resultRDY), .multRDY(multReady), .divRDY(divReady));
//     // multdiv multiplyDivide(.data_operandA(ALU_opA), .data_operandB(rt_ALU_opB), .ctrl_MULT(op_mult), .ctrl_DIV(op_div), 
//     // .clock(clock), .data_result(multdiv_result), .data_exception(multdiv_exception), .data_resultRDY(multdiv_resultRDY), .multRDY(multReady), .divRDY(divReady));
//     multdiv multiplyDivide(.data_operandA(ALU_opA), .data_operandB(rt_ALU_opB), .ctrl_MULT(op_mult), .ctrl_DIV(op_div), 
//     .clock(clock), .data_result(multdiv_result), .data_exception(multdiv_exception), .data_resultRDY(multdiv_resultRDY), .multRDY(multReady), .divRDY(divReady));

//     assign multdiv_stall = multdiv_resultRDY ? 1'b0 : DX_LATCH_IR_out[31:27] == 5'b00000 && ((DX_LATCH_IR_out[6:2] == 5'b00110) || (DX_LATCH_IR_out[6:2] == 5'b00110));

// // multdiv_stall = data_resultRDY ? 1'b0 : DX_IR_out[31:27] == 5'b00000 && (DX_IR_out[6:2] == 5'b00110) || (DX_IR_out[6:2] == 5'b00110)
    

// // **** P/W LATCH ****
//     // Stores multdiv result and IR
//     regFallingEdge PW_LATCH_MULTDIVRESULT(.data(multdiv_result), .clk(clock), .reset(reset), .enable(multdiv_resultRDY), .out(multdiv_result_pw));
//     regFallingEdge PW_LATCH_IR(.data(DX_LATCH_IR_out), .clk(clock), .reset(reset), .enable(|(DX_LATCH_IR_out)), .out(PW_LATCH_IR_out));
//     // enable condition is that a NOP hasn't been inserted, so if IR is all 0 the IR cannot be updated
//     dffe_fallEdge PW_LATCH_MULT(.q(multdiv_resultRDY_pw), .d(multdiv_resultRDY), .clk(clock), .en(1'b1), .clr(reset));
//     dffe_fallEdge PW_LATCH_EXC(.q(pw_multdiv_exception), .d(multdiv_exception), .clk(clock), .en(1'b1), .clr(reset));
//     dffe_fallEdge PW_LATCH_MULTRDY(.q(pw_multReady), .d(multReady), .clk(clock), .en(1'b1), .clr(reset));
//     dffe_fallEdge PW_LATCH_DIVRDY(.q(pw_divReady), .d(divReady), .clk(clock), .en(1'b1), .clr(reset));


//     assign pw_writereg = (pw_multdiv_exception && multdiv_resultRDY_pw) ? 5'b11110 : PW_LATCH_IR_out[26:22];
//     assign pw_writedata = (pw_multdiv_exception && multdiv_resultRDY_pw && pw_multReady) ? {{(29){1'b0}}, {1'b1}, {(2){1'b0}}} : ((pw_multdiv_exception && multdiv_resultRDY_pw && pw_divReady) ? {{(29){1'b0}}, {1'b1}, {1'b0}, {1'b1}} : multdiv_result_pw);



// // **** X/M LATCH ****
//     regFallingEdge XM_LATCH_ALUout(.data(ALU_out), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(XM_LATCH_O));
//     regFallingEdge XM_LATCH_opB(.data(DX_LATCH_B), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(XM_LATCH_B));
//     regFallingEdge XM_LATCH_ir(.data(XM_LATCH_IR_in), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(XM_LATCH_IR));
//     dffe_fallEdge XM_LATCH_aluEXC(.q(alu_overflow_xm), .d(alu_overflow), .clk(clock), .en(!multdiv_stall), .clr(reset));
//     regFallingEdge XM_LATCH_PC(.data(DX_LATCH_PC_out), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(XM_LATCH_PC_out));


//     // stores alu output, operand b, instruction (not pc here)


    
// // **** DMEM ****

//     // DMEM data bypassing: select bypassed input if MW rd is equal to XM rd and sw insn
//     assign xm_sw_true = (XM_LATCH_IR[31:27] == 5'b00111);
//     assign xm_lw_true = (XM_LATCH_IR[31:27] == 5'b01000);
//     // rd is written to in alu ops, addi, lw
//     assign xm_rdWritten_mw_alu = (MW_LATCH_IR[31:27] == 5'b00000) || (MW_LATCH_IR[31:27] == 5'b00101);
//     assign xm_rdWritten_mw_lw = (MW_LATCH_IR[31:27] == 5'b01000);
//     assign xm_sw_bypass = (XM_LATCH_IR[26:22] == mw_writereg) && (XM_LATCH_IR[26:22] != 5'b0); 

//     assign DMEM_data = (xm_sw_true && xm_sw_bypass && xm_rdWritten_mw_alu) ? mw_alu_result_exc : (xm_sw_true && xm_sw_bypass && xm_rdWritten_mw_lw) ? lw_mem_out : XM_LATCH_B;

//     // TODO: also need to check opcode of next insn? in case bits happen to match???? or deal with jump insn separately?

//     // OUTPUTS/INPUTS TO ELEMENTS IN WRAPPER MODULE
//     assign address_dmem = XM_LATCH_O;
//     assign data = DMEM_data;
//     assign wren = (XM_LATCH_IR[31:27] == 5'b00111); // TODO: update to xm_sw_true
//     assign DMEM_out = q_dmem;


//     // ALL REDUNDANT, NEED FOR BYPASSING, RECALCUALTE IN MW
//     // TODO: latch these values instead of others currently in mw latch

//     assign xm_not_nop = (XM_LATCH_IR != 32'b0);
//     assign xm_add_op = (XM_LATCH_IR[6:2] == 5'b00000)&& (XM_LATCH_IR[31:27] == 5'b00000) && xm_not_nop;
//     assign xm_sub_op = (XM_LATCH_IR[6:2]  == 5'b00001)&& (XM_LATCH_IR[31:27] == 5'b00000);
//     assign xm_addi_op = (XM_LATCH_IR[31:27] == 5'b00101);
//     assign xm_mul_op = (XM_LATCH_IR[6:2] == 5'b00110)&& (XM_LATCH_IR[31:27] == 5'b00000);
//     assign xm_div_op = (XM_LATCH_IR[6:2] == 5'b00111)&& (XM_LATCH_IR[31:27] == 5'b00000);


//     assign xm_writereg = (alu_overflow_xm && (xm_add_op || xm_sub_op || xm_addi_op)) ? 5'b11110 : XM_LATCH_IR[26:22]; 
//     assign xm_alu_result_exc = (alu_overflow_xm && xm_sub_op) ? {{(30){1'b0}}, {(2){1'b1}}} : ( (alu_overflow_xm && xm_addi_op) ? {{(30){1'b0}}, {1'b1}, {1'b0}} : ((alu_overflow_xm && xm_add_op ) ? {{(31){1'b0}}, {1'b1}} : XM_LATCH_O));
//     //assign xm_alu_result_exc = XM_LATCH_O;


// // **** M/W LATCH ****
//     regFallingEdge MW_LATCH_DMEMout(.data(DMEM_out), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(lw_mem_out));
// 	regFallingEdge MW_LATCH_ALUout(.data(XM_LATCH_O), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(regfile_input));
//     regFallingEdge MW_LATCH_ir(.data(XM_LATCH_IR), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(MW_LATCH_IR));
//     dffe_fallEdge MW_LATCH_aluEXC(.q(alu_overflow_mw), .d(alu_overflow_xm), .clk(clock), .en(!multdiv_stall), .clr(reset));
//     dffe_fallEdge MW_LATCH_mdEXC(.q(mw_multdiv_exception), .d(multdiv_resultRDY_pw), .clk(clock), .en(!multdiv_stall), .clr(reset));
//     regFallingEdge MW_LATCH_PC(.data(XM_LATCH_PC_out), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(MW_LATCH_PC_out));
//     regFallingEdge MW_LATCH_MULTDIV(.data(pw_writedata), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(multdiv_result_mw));
//     regFallingEdge_5bit MW_LATCH_MULTDIVREG(.data(pw_writereg), .clk(clock), .reset(reset), .enable(!multdiv_stall), .out(mw_multdiv_writereg));
//     dffe_fallEdge MW_LATCH_MULTDIVREADY(.q(multdiv_resultRDY_mw), .d(multdiv_resultRDY_pw), .clk(clock), .en(!multdiv_stall), .clr(reset));

//     assign mw_not_nop = (MW_LATCH_IR != 32'b0);
//     assign x_add_op = (MW_LATCH_IR[6:2] == 5'b00000)&& (MW_LATCH_IR[31:27] == 5'b00000) && mw_not_nop;
//     assign x_sub_op = (MW_LATCH_IR[6:2]  == 5'b00001)&& (MW_LATCH_IR[31:27] == 5'b00000);
//     assign x_addi_op = (MW_LATCH_IR[31:27] == 5'b00101);



//     assign mw_sw_true = (MW_LATCH_IR[31:27] == 5'b00111);
//     assign mw_lw_true = (MW_LATCH_IR[31:27] == 5'b01000);
//     assign mw_setx_true = (MW_LATCH_IR[31:27] == 5'b10101);

//     assign mw_mul_op = (MW_LATCH_IR[6:2] == 5'b00110)&& (MW_LATCH_IR[31:27] == 5'b00000);
//     assign mw_div_op = (MW_LATCH_IR[6:2] == 5'b00111)&& (MW_LATCH_IR[31:27] == 5'b00000);


//     assign mw_writereg = (alu_overflow_mw && (x_add_op || x_sub_op || x_addi_op)) ? 5'b11110 : MW_LATCH_IR[26:22]; 
//     assign mw_alu_result_exc = (alu_overflow_mw && x_sub_op) ? {{(30){1'b0}}, {(2){1'b1}}} : ( (alu_overflow_mw && x_addi_op) ? {{(30){1'b0}}, {1'b1}, {1'b0}} : ((alu_overflow_mw && x_add_op ) ? {{(31){1'b0}}, {1'b1}} : regfile_input));

//     assign mw_target_extended = {{5{MW_LATCH_IR[26]}}, MW_LATCH_IR[26:0]};

//     assign mw_jal_true = (MW_LATCH_IR[31:27] == 5'b00011);
//     assign mw_jr_true = (MW_LATCH_IR[31:27] == 5'b00100);



// // ** HAZARD DETECTION **

//     // conditions to check 
//     // f/d latch source register 1 == d/x latch destination register
//     // f/d latch source register 2 == d/x latch destination register
//     // f/d latch source register 1 == x/m latch destination register
//     // f/d latch source register 2 == x/m latch destination register
//     // this is all one big or statement where a 1 indicates insert a nop

// // *** BYPASS LOGIC ***

//     // write nop into processor then recheck condition (check same one every time)


// endmodule

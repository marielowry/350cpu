module sllCalc(A, B, shiftamt, result);
    // logical left shift by shiftamt
    
    input [31:0] A, B;
    input [4:0] shiftamt;

    output [31:0] result;

    wire [31:0] shift_16bit_calc, shift_8bit_calc, shift_4bit_calc, shift_2bit_calc;

    // first, check for 16 bit shift, otherwise keep A
    assign shift_16bit_calc = shiftamt[4] ? {A[15:0], {16{1'b0}}} : A;

    // next, shift by 8 bits if MSB of shiftamt is 1, otherwise keep prev calc
    assign shift_8bit_calc = shiftamt[3] ? {shift_16bit_calc[23:0], {8{1'b0}}} : shift_16bit_calc ; 

    // next, shift by 4 bits if shiftamt[2] == 1, otherwise keep prev calc
    assign shift_4bit_calc = shiftamt[2] ? {shift_8bit_calc[27:0], {4{1'b0}}} : shift_8bit_calc ; 

    // next, shift by 2 bits if shiftamt[1] == 1, otherwise keep prev calc
    assign shift_2bit_calc = shiftamt[1] ? {shift_4bit_calc[29:0], {2{1'b0}}} : shift_4bit_calc ;

    // finally, shift by 2 if shiftamt[0] == 1, otherwise keep previous calculation
    assign result = shiftamt[0] ? {shift_2bit_calc[30:0], {{1'b0}}} : shift_2bit_calc;


endmodule
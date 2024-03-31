module sraCalc(A, B, shiftamt, result);
    input [31:0] A, B;
    input [4:0] shiftamt;

    output [31:0] result;

    wire [31:0] shift_16bit, shift_8bit, shift_4bit, shift_2bit;

    assign shift_16bit = shiftamt[4] ? {{16{A[31]}}, A[31:16]} : A;
    assign shift_8bit = shiftamt[3] ? {{8{shift_16bit[31]}}, shift_16bit[31:8]} : shift_16bit;
    assign shift_4bit = shiftamt[2] ? {{4{shift_8bit[31]}}, shift_8bit[31: 4]} : shift_8bit;
    assign shift_2bit = shiftamt[1] ? {{2{shift_4bit[31]}}, shift_4bit[31:2]} : shift_4bit;
    assign result = shiftamt[0] ? {shift_2bit[31], shift_2bit[31:1]} : shift_2bit;

endmodule
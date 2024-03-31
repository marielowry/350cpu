module mux_2(out, select, in0, in1);
    input select;
    input [31:0] in0, in1;
    output [31:0] out;

    // this line assigns wire out to the value of in1 if select is true, or in0 if select is false
    assign out = select ? in1:in0;
endmodule;
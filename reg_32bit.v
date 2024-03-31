module reg_32bit(data, clk, reset, enable, out);

    input [31:0] data;
    input clk, reset, enable;

    output [31:0] out;

    //dffe module: dffe_ref (q, d, clk, en, clr);
    //generate 32 D flip flops in a register
    genvar i;
    generate
        for (i=0; i<32; i = i+1) begin: genDFF
            dffe_ref dff(out[i], data[i], clk, enable, reset);
        end
    endgenerate

endmodule
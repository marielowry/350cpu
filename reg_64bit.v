module reg_64bit(data, clk, reset, enable, out);
    input [63:0] data;
    input clk, reset, enable;

    output [63:0] out;

    genvar c;
    generate
        for (c=0; c<64; c=c+1) begin: initializeReg
            dffe_ref dff(out[c], data[c], clk, enable, reset);
        end
    endgenerate


endmodule
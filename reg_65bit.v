module reg_65bit(data, clk, reset, enable, out);
    input [64:0] data;
    input clk, reset, enable;

    output [64:0] out;

    genvar c;
    generate
        for (c=0; c<65; c=c+1) begin: initializeReg
            dffe_ref dff(out[c], data[c], clk, enable, reset);
        end
    endgenerate


endmodule
module regFallingEdge_5bit(data, clk, reset, enable, out);

    input [4:0] data;
    input clk, reset, enable;

    output [4:0] out;


    //dffe module: dffe_ref (q, d, clk, en, clr);
    //generate 32 D flip flops in a register
    genvar i;
    generate
        for (i=0; i<5; i = i+1) begin: genDFF
            dffe_fallEdge dff(out[i], data[i], clk, enable, reset);
        end
    endgenerate



endmodule
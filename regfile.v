module regfile (
	clock,
	ctrl_writeEnable, ctrl_reset, ctrl_writeReg,
	ctrl_readRegA, ctrl_readRegB, data_writeReg,
	data_readRegA, data_readRegB
);

	input clock, ctrl_writeEnable, ctrl_reset;
	input [4:0] ctrl_writeReg, ctrl_readRegA, ctrl_readRegB;
	input [31:0] data_writeReg;

	wire [31:0] writeSelect, readASelect, readBSelect, reg0_data;
	wire writeEnable;

	output [31:0] data_readRegA, data_readRegB;

	// make 32 registers with 32 instances of regOne
	// format: module regOne(data, clk, reset, enable, out);

	assign writeSelect = (ctrl_writeEnable) ? (1'b1 << ctrl_writeReg) : 32'b0;
	assign readASelect = 1'b1 << ctrl_readRegA;
	assign readBSelect = 1'b1 << ctrl_readRegB;


	// separate instance of register 0 because always reads 0
	regOne regOne_0({32'b0}, clock, ctrl_reset, writeSelect[0], reg0_data);
	assign data_readRegA = readASelect[0] ? reg0_data : 32'bz;
	assign data_readRegB = readBSelect[0] ? reg0_data : 32'bz;


	genvar c;
	generate
		for (c=1; c<32; c=c+1) begin: generate_registers
			wire [31:0] data_out;
			regOne regOne_c(data_writeReg, clock, ctrl_reset, writeSelect[c], data_out);
			assign data_readRegA = readASelect[c] ? data_out : 32'bz;
			assign data_readRegB = readBSelect[c] ? data_out : 32'bz;
		end
	endgenerate



endmodule

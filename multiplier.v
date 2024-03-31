module multiplier(data_operandA, data_operandB, clock, data_result, data_exception, data_resultRDY, ctrl_MULT);

    input [31:0] data_operandA, data_operandB;
    input clock, ctrl_MULT;

    output [31:0] data_result;
    output data_exception, data_resultRDY;

    wire reset, enable, shiftTrue, doNothing, productRegControl, initializing, clearCounter, wrongSign;
    wire w1, w2, w3, w4;
    wire [5:0] counter; 
    wire [64:0] productRegInput, productRegOutput, shiftProductReg;
    wire [31:0] shiftedAddInput, addIn1, mRegOutput, addOutput, shiftedAddOutput;

    // module structure: reg_65bit(data, clk, reset, enable, out);
    // initialize two registers with multiplicand and product/multiplier
    reg_65bit productReg(productRegInput, clock, reset, 1'b1, productRegOutput);
    reg_32bit multiplicandReg(data_operandA, clock, reset, 1'b1, mRegOutput);

    // shift by 1 if needed for adder input
    assign shiftTrue = (((~productRegOutput[2]) && productRegOutput[1]) && productRegOutput[0]) || (productRegOutput[2] && (~productRegOutput[1]) && (~productRegOutput[0]));
    // select correct shifter input
    assign shiftedAddInput = shiftTrue ? mRegOutput<<1 : mRegOutput;

    // set adder operand b to 0 for 000 or 111 case
    assign doNothing = &productRegOutput[2:0] || (&(~productRegOutput[2:0]));
    assign addIn1 = doNothing ? 32'b0 : shiftedAddInput;

    // add if needed, check last 3 bits of product register
    // add when clock high
    // addCalc structure: addCalc(A, B, S, isNotEqual, isLessThan, overflow, subTrue);
    // TODO: fix wires and logic for isnotequal, islessthan, overflow
    addCalc addMultiplicand(productRegOutput[64:33], addIn1, addOutput, w1, w2, w3, productRegOutput[2]);

    // shift output
    assign shiftProductReg = $signed({{addOutput}, {productRegOutput[32:0]}}) >>> 2;

    // select productRegInput as either output of adder or initial values
    assign initializing = &(~counter); // 1 if counter is 00000
    assign productRegInput = initializing ? {{32'b0}, {data_operandB}, {1'b0}} : shiftProductReg;

    // COUNTER
    // increment on each clock cycle
    //counter counter1(counter, clock, clearCounter);
    counter counter1(counter, clock, ctrl_MULT);
    // when counter is 10001 set output result and stop operations
    and checkIfReady(data_resultRDY, ~counter[5], counter[4], ~counter[3], ~counter[2], ~counter[1], counter[0]);

    // when result ready, set output to the product register output
    assign data_result = data_resultRDY ? productRegOutput[32:1] : 32'b0;

    xor checkSign(wrongSign, data_operandA[31], data_operandB[31], (productRegOutput[64]|productRegOutput[32]));
    // check if the data is not all 0/1 OR if the sign is wrong
    assign data_exception =  ~(&productRegOutput[64:33] || ~(|productRegOutput[64:33]))|| (wrongSign && !(&(~data_operandA) || &(~data_operandB)));
    



endmodule
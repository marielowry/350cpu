module divider_faster(data_operandA, data_operandB, clock, data_result, data_exception, data_resultRDY, ctrl_DIV);

    input [31:0] data_operandA, data_operandB;
    input clock, ctrl_DIV;

    output [31:0] data_result;
    output data_exception, data_resultRDY;

    wire reset, enable, shiftTrue, doNothing, productRegControl, initializing, wrongSign;
    wire A_negative, B_negative, negResult;
    wire w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13;
    wire [5:0] counter; 
    wire [63:0] productRegInput, productRegOutput, shiftProductReg, shiftedAddInput;
    wire [31:0] addIn1, mRegOutput, addOutput, shiftedAddOutput, A_switchedSign, B_switchedSign, A_unsigned, B_unsigned, output_Signed;

    // NEW WIRES
    wire [31:0] addOutput1, addOutput2, output_unsigned, output_unsigned_incremented;

    // A is negative if data_operandA == 1
    // if A is negative, make A positive
    addCalc Zero_minus_A(32'b0, data_operandA, A_switchedSign, w5, w6, w7, 1'b1);
    assign A_unsigned = data_operandA[31] ? A_switchedSign : data_operandA;

    // switch sign of B if necessary
    addCalc Zero_minus_B(32'b0, data_operandB, B_switchedSign, w8, w9, w10, 1'b1);
    assign B_unsigned = data_operandB[31] ? B_switchedSign : data_operandB;

    // module structure: reg_65bit(data, clk, reset, enable, out);
    // initialize two registers with multiplicand and product/multiplier
    reg_64bit productReg(productRegInput, clock, reset, 1'b1, productRegOutput);
    reg_32bit divisorReg(B_unsigned, clock, reset, 1'b1, mRegOutput);

    // shift left by 1
    assign shiftedAddInput = productRegOutput<<1;

    // subtract A = A - M
    // addCalc structure: addCalc(A, B, S, isNotEqual, isLessThan, overflow, subTrue);
    //addCalc subDivisor(shiftedAddInput[63:32], mRegOutput, addOutput, w1, w2, w3, 1'b1);

    // NEW: If sign bit of A is 0 (A positive), A = A-M (remove prev adder)
    addCalc sub1Divisor(shiftedAddInput[63:32], mRegOutput, addOutput1, w1, w2, w3, 1'b1);

    // NEW: If sign bit of A is 1 (A negative), A = A+M
    addCalc subDivisor(shiftedAddInput[63:32], mRegOutput, addOutput2, w1, w2, w3, 1'b0);

    // mux where select bit is MSB of A, select between addOutput1 and addOutput2
    assign addOutput = shiftedAddInput[63] ? addOutput2 : addOutput1;

    assign initializing = &(~counter); // 1 if counter is 00000
    // product register input checks between initial value, added value, or restored value
    assign productRegInput = initializing ? {{32'b0}, {A_unsigned}} : (addOutput[31] ? shiftedAddInput : {{addOutput}, {shiftedAddInput[31:1]}, {1'b1}});
    //assign productRegInput = initializing ? {{32'b0}, {data_operandB}, {1'b0}} : productRegOutput;    

    // COUNTER
    // increment on each clock cycle
    counter counter2(counter, clock, ctrl_DIV);
    // when counter is 10001 set output result and stop operations
    // NEW: result ready at 01001 instead of 10001??
    and checkIfReady(data_resultRDY, counter[5], ~counter[4], ~counter[3], ~counter[2], ~counter[1], counter[0]);
    // ready if counter == N


    // NEW: if sign bit of A is 1 at this point, A = A + M
    // adder which adds 1 to product reg output with data_resultRDY and sign bit of A as input
    // if data_resultRDY AND sign bit of A is 1, 

    addCalc incrementA(mRegOutput, productRegOutput[31:0], output_unsigned_incremented, w11, w12, w13, 1'b0);
    assign output_unsigned = (data_resultRDY && productRegOutput[31]) ? output_unsigned_incremented : productRegOutput[31:0]; // THIS IS WRONG



    // switch result to negative if necessary
    //addCalc switchResultSign(32'b0, productRegOutput[31:0], output_Signed, w11, w12, w13, 1'b1);
    addCalc switchResultSign(32'b0, output_unsigned, output_Signed, w11, w12, w13, 1'b1);



    // when result ready, set output to the product register output
    assign negResult = (data_operandA[31] && ~data_operandB[31]) || (~data_operandA[31] && data_operandB[31]);
    assign data_result = data_resultRDY ? (negResult ? output_Signed : productRegOutput[31:0]) : 32'b0;

    // xor checkSign(wrongSign, data_operandA[31], data_operandB[31], (productRegOutput[63]|productRegOutput[31]));
    // check if the data is not all 0/1 OR if the sign is wrong
    // assign data_exception =  ~(&productRegOutput[63:31] || ~(|productRegOutput[63:31]))|| (wrongSign && !(&(~data_operandA) || &(~data_operandB)));
    // DIFFERENT EXCEPTION CONDITIONS

    // CHECK FOR: divide by 0 --> DOESN'T APPLY WITH 0 as A
    assign data_exception = (&(~data_operandB));



endmodule
module multdiv(
	data_operandA, data_operandB, 
	ctrl_MULT, ctrl_DIV, 
	clock, 
	data_result, data_exception, data_resultRDY, multRDY, divRDY);

    input [31:0] data_operandA, data_operandB;
    input ctrl_MULT, ctrl_DIV, clock;

    output [31:0] data_result;
    output data_exception, data_resultRDY, multRDY, divRDY;

    wire [31:0] divResult, multResult;
    wire multException, divException, multReady, divReady, multTrue, divTrue, ctrl_MULT_onecyc, ctrl_DIV_onecyc, ctrl_MULT_dff, ctrl_DIV_dff;

    wire enable;

    //dffe_ref (q, d, clk, en, clr);    
    dffe_ref multTrue_dff(multTrue, ctrl_MULT, clock, ctrl_MULT, ctrl_DIV);

    dffe_ref multConvert(ctrl_MULT_dff, ctrl_MULT, clock, 1'b1, !ctrl_MULT);
    dffe_ref divConvert(ctrl_DIV_dff, ctrl_DIV, clock, 1'b1, !ctrl_DIV);

    and multONECYCLE(ctrl_MULT_onecyc, !ctrl_MULT_dff, ctrl_MULT);
    and divONECYCLE(ctrl_DIV_onecyc, !ctrl_DIV_dff, ctrl_DIV);



    multiplier doMult(data_operandA, data_operandB, clock, multResult, multException, multReady, ctrl_MULT_onecyc);
    //divider doDiv(data_operandA, data_operandB, clock, divResult, divException, divReady, ctrl_DIV);
    divider_faster doDiv(data_operandA, data_operandB, clock, divResult, divException, divReady, ctrl_DIV_onecyc);

    assign data_result = multTrue ? multResult : divResult;
    assign data_exception = multTrue ? multException : divException;
    assign data_resultRDY = multTrue ? multReady : divReady;

    assign multRDY = multReady;
    assign divRDY = divReady;

endmodule
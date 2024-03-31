module addCalc(A, B, S, isNotEqual, isLessThan, overflow, subTrue);
    // full 32bit adder

    input [31:0] A, B ;
    input subTrue;

    output [31:0] S ;
    output isNotEqual, isLessThan, overflow;

    wire c1, c2, c3, c4, P0, P1, P2, P3, G0, G1, G2, G3;
    wire [7:0] addCalc0, addCalc1, AddCalc2, AddCalc3;
    wire c8, c16, c24, c32;
    wire w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15, w16;
    wire ovfCheck1_add, ovfCheck2_add, ovfCheck1_sub, ovfCheck2_sub, addTrue;
    wire [31:0] Binv, B1;

    // calculate not of B
    not B31_inv(Binv[31], B[31]);
    not B30_inv(Binv[30], B[30]);
    not B29_inv(Binv[29], B[29]);
    not B28_inv(Binv[28], B[28]);
    not B27_inv(Binv[27], B[27]);
    not B26_inv(Binv[26], B[26]);
    not B25_inv(Binv[25], B[25]);
    not B24_inv(Binv[24], B[24]);
    not B23_inv(Binv[23], B[23]);
    not B22_inv(Binv[22], B[22]);
    not B21_inv(Binv[21], B[21]);
    not B20_inv(Binv[20], B[20]);
    not B19_inv(Binv[19], B[19]);
    not B18_inv(Binv[18], B[18]);
    not B17_inv(Binv[17], B[17]);
    not B16_inv(Binv[16], B[16]);
    not B15_inv(Binv[15], B[15]);
    not B14_inv(Binv[14], B[14]);
    not B13_inv(Binv[13], B[13]);
    not B12_inv(Binv[12], B[12]);
    not B11_inv(Binv[11], B[11]);
    not B10_inv(Binv[10], B[10]);
    not B9_inv(Binv[9], B[9]);
    not B8_inv(Binv[8], B[8]);
    not B7_inv(Binv[7], B[7]);
    not B6_inv(Binv[6], B[6]);
    not B5_inv(Binv[5], B[5]);
    not B4_inv(Binv[4], B[4]);
    not B3_inv(Binv[3], B[3]);
    not B2_inv(Binv[2], B[2]);
    not B1_inv(Binv[1], B[1]);
    not B0_inv(Binv[0], B[0]);

    // select correct bits for b
    assign B1 = subTrue ? Binv : B;


    // calculate sums
    addCalc_8bit Add0(.A(A[7:0]), .B(B1[7:0]), .Cin(subTrue), .S(S[7:0]), .Cout(c1), .P(P0), .G(G0));
    addCalc_8bit Add1(.A(A[15:8]), .B(B1[15:8]), .Cin(c8), .S(S[15:8]), .Cout(c2), .P(P1), .G(G1));
    addCalc_8bit Add2(.A(A[23:16]), .B(B1[23:16]), .Cin(c16), .S(S[23:16]), .Cout(c3), .P(P2), .G(G2));
    addCalc_8bit Add3(.A(A[31:24]), .B(B1[31:24]), .Cin(c24), .S(S[31:24]), .Cout(c4), .P(P3), .G(G3));


    // calculate carry out between 8bit blocks
    // c8 calculation
    and P0_c0(w1, P0, subTrue);
    or CALC_c8(c8, w1, G0);

    // c16 calculation
    and P1_G0(w2, P1, G0);
    and P1_P0_c0(w3, P1, P0, subTrue);
    or CALC_c16(c16, G1, w2, w3);

    // c24 calculation
    and P2_G1(w4, P2, G1);
    and P2_P1_G0(w5, P2, P1, G0);
    and P2_P1_P0_c0(w6, P2, P1, P0, subTrue);
    or CALC_c24(c24, G2, w4, w5, w6);

    // c32 calculation
    and P3_G2(w7, P3, G2);
    and P3_P2_G1(w8, P3, P2, G1);
    and P3_P2_P1_G0(w9, P3, P2, P1, G0);
    and P3_P2_P1_P0_c0(w10, P3, P2, P1, P0, subTrue);
    or CALC_c32(c32, G3, w7, w8, w9, w10);

    // IS LESS THAN CALCULATION
    not notB31(w11, B[31]);
    and A_notB(w12, A[31], w11);
    and A_C(w13, A[31], S[31]);
    and notB_C(w14, w11, S[31]);
    or CALC_LESS_THAN(isLessThan, w12, w13, w14);

    // EQUALITY CALCULATION
    // TODO: figure out a better way to do this
    // TODO: also make sure solution can't be all 1s
    or CALC_EQUALS(isNotEqual, S[0], S[1], S[2], S[3], S[4], S[5], S[6], S[7], S[8], S[9], S[10], S[11], S[12], S[13], S[14], S[15], S[16], S[17], S[18], S[19], S[20], S[21], S[22], S[23], S[24], S[25], S[26], S[27], S[28], S[29], S[30], S[31]);

    // Overflow Calculation
    // overflow occurs when signs of operands match and differ from sign of answer
    // FIRST CHECK WITH ADDITION
    // condition 1: both inputs are positive, first bit 0, and output is negative, first bit 1
    not notA31(w15, A[31]);
    not addtrue(addTrue, subTrue);
    and posOvf(ovfCheck1_add, w15, w11, S[31], addTrue); 
    // condition 2: both inputs are negative, first bit 1, and output is positive, first bit 0
    not notS31(w16, S[31]);
    and negOvf(ovfCheck2_add, A[31], B[31], w16, addTrue);
    // select correct condition
    and negOvf_sub(ovfCheck1_sub, A[31], Binv[31], w16, subTrue);
    and posOvf_sub(ovfCheck2_sub, w15, B[31], S[31], subTrue);
    // total calculation
    or CALC_ADD_OVERFLOW(overflow, ovfCheck1_add, ovfCheck2_add, ovfCheck1_sub, ovfCheck2_sub);


endmodule
module addCalc_8bit(A, B, Cin, S, Cout, P, G);

input [7:0] A, B;
input Cin;

output [7:0] S;
output Cout, P, G;

// g and p values stored as wires, because all used for intermediate calculations
wire g0, g1, g2, g3, g4, g5, g6, g7;
wire p0, p1, p2, p3, p4, p5, p6, p7;
wire c1, c2, c3, c4, c5, c6, c7;
wire w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15, w16, w17, w18, w19, w20, w21, w22, w23, w24, w25, w26, w27, w28, w29, w30, w31, w32, w33, w34, w35;

// calculate all g values
and CALC_g0(g0, A[0], B[0]);
and CALC_g1(g1, A[1], B[1]);
and CALC_g2(g2, A[2], B[2]);
and CALC_g3(g3, A[3], B[3]);
and CALC_g4(g4, A[4], B[4]);
and CALC_g5(g5, A[5], B[5]);
and CALC_g6(g6, A[6], B[6]);
and CALC_g7(g7, A[7], B[7]);

// calculate all p values
or CALC_p0(p0, A[0], B[0]);
or CALC_p1(p1, A[1], B[1]);
or CALC_p2(p2, A[2], B[2]);
or CALC_p3(p3, A[3], B[3]);
or CALC_p4(p4, A[4], B[4]);
or CALC_p5(p5, A[5], B[5]);
or CALC_p6(p6, A[6], B[6]);
or CALC_p7(p7, A[7], B[7]);

// calculate c1
and p0_and_c0(w0, p0, Cin);
or CALC_c1(c1, g0, w0);

// calculate c2
and p1_g0(w1, p1, g0);
and p1_p0_c0(w2, p1, p0, Cin);
or CALC_c2(c2, g1, w1, w2);

// calculate c3
and p2_g1(w3, p2, g1);
and p2_p1_g0(w4, p2, p1, g0);
and p2_p1_p0_c0(w5, p2, p1, p0, Cin);
or CALC_c3(c3, g2, w3, w4, w5);

// calculate c4
and p3_g2(w6, p3, g2);
and p3_p2_g1(w7, p3, p2, g1);
and p3_p2_p1_g0(w8, p3, p2, p1, g0);
and p3_p2_p1_p0_c0(w9, p3, p2, p1, p0, Cin);
or CALC_c4(c4, g3, w6, w7, w8, w9);

// calculate c5
and p4_g3(w10, p4, g3);
and p4_p3_g2(w11, p4, p3, g2);
and p4_p3_p2_g1(w12, p4, p3, p2, g1);
and p4_p3_p2_p1_g0(w13, p4, p3, p2, p1, g0);
and p4_p3_p2_p1_p0_c0(w14, p4, p3, p2, p2, p1, p0, Cin);
or CALC_c5(c5, g4, w10, w11, w12, w13, w14);

// calculate c6
and p5_g4(w15, p5, g4);
and p5_p4_g3(w16, p5, p4, g3);
and p5_p4_p3_g2(w17, p5, p4, p3, g2);
and p5_p4_p3_p2_g1(w18, p5, p4, p3, p2, g1);
and p5_p4_p3_p2_p1_g0(w19, p5, p4, p3, p2, p1, g0);
and p5_p4_p3_p2_p1_p0_c0(w20, p5, p4, p3, p2, p1, p0, Cin);
or CALC_c6(c6, g5, w15, w16, w17, w18, w19, w20);

// calculate c7
and p6_g5(w21, p6, g5);
and p6_p5_g4(w22, p6, p5, g4);
and p6_p5_p4_g3(w23, p6, p5, p4, g3);
and p6_p5_p4_p3_g2(w24, p6, p5, p4, p3, g2);
and p6_p5_p4_p3_p2_g1(w25, p6, p5, p4, p3, p2, g1);
and p6_p5_p4_p3_p2_p1_g0(w26, p6, p5, p4, p3, p2, p1, g0);
and p6_p5_p4_p3_p2_p1_p0_c0(w27, p6, p5, p4, p3, p2, p1, p0, Cin);
or CALC_c7(c7, g6, w21, w22, w23, w24, w25, w26, w27);

// calculate cout
and p7_g6(w28, p7, g6);
and p7_p6_g5(w29, p7, p6, g5);
and p7_p6_p5_g4(w30, p7, p6, p5, g4);
and p7_p6_p5_p4_g3(w31, p7, p6, p5, p4, g3);
and p7_p6_p5_p4_p3_g2(w32, p7, p6, p5, p4, p3, g2);
and p7_p6_p5_p4_p3_p2_g1(w33, p7, p6, p5, p4, p3, p2, g1);
and p7_p6_p5_p4_p3_p2_p1_g0(w34, p7, p6, p5, p4, p3, p2, p1, g0);
and p7_p6_p5_p4_p3_p2_p1_p0_c0(w35, p7, p6, p5, p4, p3, p2, p1, p0, Cin);
or CALC_Cout(Cout, g7, w28, w29, w30, w31, w32, w33, w34, w35);

// calculate P and G
and CALC_P(P, p0, p1, p2, p3, p4, p5, p6, p7);
// might need to change this line w new wires
or CALC_G(G, g7, w28, w29, w30, w31, w32, w33, w34);


// calculate sum
xor CALC_S0(S[0], A[0], B[0], Cin);
xor CALC_S1(S[1], A[1], B[1], c1);
xor CALC_S2(S[2], A[2], B[2], c2);
xor CALC_S3(S[3], A[3], B[3], c3);
xor CALC_S4(S[4], A[4], B[4], c4);
xor CALC_S5(S[5], A[5], B[5], c5);
xor CALC_S6(S[6], A[6], B[6], c6);
xor CALC_S7(S[7], A[7], B[7], c7);


endmodule
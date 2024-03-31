module orCalc(A, B, result);

    input [31:0] A, B;

    output [31:0] result;

    wire r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31;

    or res0(r0, A[0], B[0]);
    or res1(r1, A[1], B[1]);
    or res2(r2, A[2], B[2]);
    or res3(r3, A[3], B[3]);
    or res4(r4, A[4], B[4]);
    or res5(r5, A[5], B[5]);
    or res6(r6, A[6], B[6]);
    or res7(r7, A[7], B[7]);
    or res8(r8, A[8], B[8]);
    or res9(r9, A[9], B[9]);
    or res10(r10, A[10], B[10]);
    or res11(r11, A[11], B[11]);
    or res12(r12, A[12], B[12]);
    or res13(r13, A[13], B[13]);
    or res14(r14, A[14], B[14]);
    or res15(r15, A[15], B[15]);
    or res16(r16, A[16], B[16]);
    or res17(r17, A[17], B[17]);
    or res18(r18, A[18], B[18]);
    or res19(r19, A[19], B[19]);
    or res20(r20, A[20], B[20]);
    or res21(r21, A[21], B[21]);
    or res22(r22, A[22], B[22]);
    or res23(r23, A[23], B[23]);
    or res24(r24, A[24], B[24]);
    or res25(r25, A[25], B[25]);
    or res26(r26, A[26], B[26]);
    or res27(r27, A[27], B[27]);
    or res28(r28, A[28], B[28]);
    or res29(r29, A[29], B[29]);
    or res30(r30, A[30], B[30]);
    or res31(r31, A[31], B[31]);

    assign result = {r31, r30, r29, r28, r27, r26, r25, r24, r23, r22, r21, r20, r19, r18, r17, r16, r15, r14, r13, r12, r11, r10, r9, r8, r7, r6, r5, r4, r3, r2, r1, r0};
 


endmodule
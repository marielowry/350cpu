module counter(Q, clock, clr);

    input clock, clr;
    output [5:0] Q;
    wire w1, w2, w3, w4;

    // dffe_ref (q, d, clk, en, clr)

    tff tff0(Q[0], 1'b1, clock, clr);
    tff tff1(Q[1], Q[0], clock, clr);

    and and1(w1, Q[0], Q[1]);
    tff tff2(Q[2], w1, clock, clr);

    and and2(w2, Q[0], Q[1], Q[2]);
    tff tff3(Q[3], w2, clock, clr);

    and and3(w3, Q[0], Q[1], Q[2], Q[3]);
    tff tff4(Q[4], w3, clock, clr);

    and and4(w4, Q[0], Q[1], Q[2], Q[3], Q[4]);
    tff tff5(Q[5], w4, clock, clr);

endmodule

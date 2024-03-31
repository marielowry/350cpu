module tff(Q, T, clock, clr); 

    input clock, clr, T;
    output Q;
    wire w1, w2, w3;

    and and1(w1, ~T, Q);
    and and2(w2, T, ~Q);
    or or1(w3, w1, w2);

    dffe_ref dff1(Q, w3, clock, 1'b1, clr);


endmodule
import "DPI-C" function void ifu(input int ifu_cnt); 
module test(input[31:0] cnt);
    always_comb
        ifu(cnt);
endmodule

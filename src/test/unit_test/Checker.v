module Checker(
    input clock,
    input reset,
    input finish,
    input ret
);
import "DPI-C" function void check(input int finish,input int ret);

always @(*) begin
    if(finish)
        check(finish,ret);
end
endmodule

module TraceGen(
    input clock,
    input reset,
    input valid,
    output [31:0]pc     , 
    output [31:0]target , 
    output [1:0]br_type     , 
    output taken        , 
    output [4:0]rs1   , 
    output [4:0]rd     
);
import "DPI-C" function void Br_check(output int pc,output int target,output int type,output int taken,output int rs1,output int rd,input int valid);
always @(*) begin
    if(valid)
        Br_check(pc,target,br_type,taken,rs1,rd,valid);
end
endmodule
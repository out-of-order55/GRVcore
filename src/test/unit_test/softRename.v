module softRename(
    input clock,
    input reset,
    input [1:0]  reqs,
    input [9:0]  lrs1,
    input [9:0]  lrs2,
    input [9:0]  ldst,
    output [11:0]  prs1,
    output [11:0]  prs2,
    output [11:0]  pdst,  
    output [11:0]  old_pdst 
);
import "DPI-C" function void softRename(input int reqs,input int lrs1s,input int lrs2s,input int ldsts,
            output int old_pdsts,output int prs1s,output int prs2s,output int pdsts);
always @(*) begin
    softRename(reqs,lrs1,lrs2,ldst,old_pdst,prs1,prs2,pdst);
end
endmodule

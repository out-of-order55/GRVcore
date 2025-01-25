module Difftest(
    input clock,
    input reset,
    input [7:0]  commit_num,
    input [3:0]  commit_valid,
    input [31:0]  commit_pc1,
    input [31:0]  commit_pc2,
    input [31:0]  commit_data1,
    input [31:0]  commit_data2
);

import "DPI-C" function void super_difftest_step(input int pc1, input int pc2,input int valid,
input int data1,input int data2,input int num)
always @(*) begin
    super_difftest_step(commit_pc1,commit_pc2,valid,data1,data2,num);
end
endmodule
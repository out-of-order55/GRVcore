module SIMSRAM(
  input [31:0] raddr,
  input [31:0] waddr,
  input clock,
  input reset,
  input ren,
  input wen,
  input[3:0] wstrb,
  input[31:0]wdata,
  output reg [31:0] rdata
);
import "DPI-C" function void SRAM(input int raddr, output int rdata,input int ren,input int wen,input int waddr,input int wstrb,input int wdata);
always @(*) begin
  SRAM(raddr,rdata,ren,wen,waddr,wstrb,wdata);
end
endmodule

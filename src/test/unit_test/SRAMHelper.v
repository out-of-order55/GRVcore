module SRAMHelper(
  input [31:0] raddr,
  input clock,
  input reset,
  input ren,
  output reg [31:0] rdata
);
import "DPI-C" function void SRAM_read(input int raddr, output int rdata);
always @(*) begin
  if (ren) SRAM_read(raddr, rdata);
  else rdata = 0;
end
endmodule

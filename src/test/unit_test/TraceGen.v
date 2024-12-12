module TraceGen(
    input clock,
    input reset,
    input valid3,
    input valid0,
    input [31:0] s0_ptr,
    input [31:0] s3_ptr,
    output [31:0]s0_pc,
    output [31:0]pc     , 
    output [31:0]target , 
    output [1:0]br_type     , 
    output taken        , 
    output [4:0]rs1   , 
    output [4:0]rd     
);
import "DPI-C" function void Br_check(output int s0_pc,output int pc,output int target,
            output int br_type,output int taken,output int rs1,output int rd,
            input int valid3,input int valid0,input int s0_ptr,input int s3_ptr);
    wire [31:0] t_pc        ; 
    wire [31:0] t_s0_pc     ; 
    wire [31:0] t_target    ; 
    wire [1:0]  t_br_type   ; 
    wire        t_taken     ;
    wire [4:0]  t_rs1       ;
    wire [4:0]  t_rd        ;
always @(*) begin
    Br_check(t_s0_pc,t_pc,t_target,t_br_type,t_taken,t_rs1,t_rd,valid3,valid0,s0_ptr,s3_ptr);
end
assign pc     = valid3?t_pc     :'b0;
assign target = valid3?t_target :'b0;
assign br_type= valid3?t_br_type:'b0;
assign taken  = valid3?t_taken  :'b0;
assign rs1    = valid3?t_rs1    :'b0;
assign rd     = valid3?t_rd     :'b0;
assign s0_pc  = valid0?t_s0_pc  :'b0;
endmodule

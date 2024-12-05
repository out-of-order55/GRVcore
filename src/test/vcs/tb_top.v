
`timescale 1ns / 1ns

module tb_top();

reg clock,reset;



initial begin
    clock=0;
    reset=1;
    #100;
    reset =0;
    #1000;
    $finish;
end

always #(1) clock = ~clock;

initial	begin
	    $fsdbDumpfile("tb.fsdb");
	    $fsdbDumpvars;
end

SimTop SimTop(	// @[src/main/test.scala:47:7]
        .clock(clock),	// @[src/main/test.scala:47:7]
        .reset(reset)	// @[src/main/test.scala:47:7]
);



endmodule

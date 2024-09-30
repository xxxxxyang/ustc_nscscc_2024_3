module LogicRegFile (
    input aclk,
    input reset,
    input cmt_valid0,
    input cmt_valid1,
    input rf_wen_0,
    input rf_wen_1,
    input [31:0] debug0_wb_rf_wdata,
    input [31:0] debug1_wb_rf_wdata,
    input [ 4:0] debug0_wb_rf_wnum,
    input [ 4:0] debug1_wb_rf_wnum,
    output logic [31:0] logic_rf[0:31]
);
    reg [31:0] logic_rf_real[0:31];
    always @(posedge aclk)  begin
        if(reset) begin
            integer i;
            for (i = 0; i < 32; i = i + 1) begin
                logic_rf_real[i] <= 32'b0;
            end
        end
        else begin
            if(rf_wen_0 & cmt_valid0)  logic_rf_real[debug0_wb_rf_wnum] <= debug0_wb_rf_wdata;
            if(rf_wen_1 & cmt_valid1)  logic_rf_real[debug1_wb_rf_wnum] <= debug1_wb_rf_wdata;
        end
    end
    always @(*) begin
        integer i;
        for (i = 0; i < 32; i = i + 1) begin
            if(rf_wen_1 & cmt_valid1 & (debug1_wb_rf_wnum == i)) logic_rf[i] = debug1_wb_rf_wdata;
            else if(rf_wen_0 & cmt_valid0 & (debug0_wb_rf_wnum == i)) logic_rf[i] = debug0_wb_rf_wdata;
            else  logic_rf[i] = logic_rf_real[i];
        end
    end
    
endmodule

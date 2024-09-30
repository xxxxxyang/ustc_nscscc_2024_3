module core_top(
    input           aclk,
    input           aresetn,
    input    [ 7:0] intrpt, 
    //AXI interface 
    //read reqest
    output   [ 3:0] arid,
    output   [31:0] araddr,
    output   [ 7:0] arlen,
    output   [ 2:0] arsize,
    output   [ 1:0] arburst,
    output   [ 1:0] arlock,
    output   [ 3:0] arcache,
    output   [ 2:0] arprot,
    output          arvalid,
    input           arready,
    //read back
    input    [ 3:0] rid,
    input    [31:0] rdata,
    input    [ 1:0] rresp,
    input           rlast,
    input           rvalid,
    output          rready,
    //write request
    output   [ 3:0] awid,
    output   [31:0] awaddr,
    output   [ 7:0] awlen,
    output   [ 2:0] awsize,
    output   [ 1:0] awburst,
    output   [ 1:0] awlock,
    output   [ 3:0] awcache,
    output   [ 2:0] awprot,
    output          awvalid,
    input           awready,
    //write data
    output   [ 3:0] wid,
    output   [31:0] wdata,
    output   [ 3:0] wstrb,
    output          wlast,
    output          wvalid,
    input           wready,
    //write back
    input    [ 3:0] bid,
    input    [ 1:0] bresp,
    input           bvalid,
    output          bready,
    //debug
    input           break_point,
    input           infor_flag,
    input  [ 4:0]   reg_num,
    output          ws_valid,
    output [31:0]   rf_rdata,
    //debug info
    output [31:0] debug0_wb_pc,
    output [ 3:0] debug0_wb_rf_wen,
    output [ 4:0] debug0_wb_rf_wnum,
    output [31:0] debug0_wb_rf_wdata,

    // #ifdef CPU_2CMT
    // ,
    output [31:0] debug1_wb_pc,
    output [ 3:0] debug1_wb_rf_wen,
    output [ 4:0] debug1_wb_rf_wnum,
    output [31:0] debug1_wb_rf_wdata

    // #endif
);
reg     reset;
always @(posedge aclk) reset <= ~aresetn; 
wire    rf_wen_1;
wire    rf_wen_0;
assign  debug0_wb_rf_wen = {4{rf_wen_0}};
assign  debug1_wb_rf_wen = {4{rf_wen_1}};
wire  cmt_valid0;
wire  cmt_valid1;
wire  [63:0]  cmt_stable_counter;
wire  [7:0]  cmt_exception;
wire  [31:0]  logic_rf[0:31];

wire  [31:0]  cmt_inst0;
wire  [31:0]  cmt_inst1;
wire  [31:0]  csr_crmd_diff     ;
wire  [31:0]  csr_prmd_diff     ;
wire  [31:0]  csr_euen_diff     ;
wire  [31:0]  csr_ectl_diff     ;
wire  [31:0]  csr_estat_diff    ;
wire  [31:0]  csr_era_diff      ;
wire  [31:0]  csr_badv_diff     ;
wire  [31:0]  csr_eentry_diff   ;
wire  [31:0]  csr_tlbidx_diff   ;
wire  [31:0]  csr_tlbehi_diff   ;
wire  [31:0]  csr_tlbelo0_diff  ;
wire  [31:0]  csr_tlbelo1_diff  ;
wire  [31:0]  csr_asid_diff     ;
wire  [31:0]  csr_pgdl_diff     ;
wire  [31:0]  csr_pgdh_diff     ;
wire  [31:0]  csr_save0_diff    ;
wire  [31:0]  csr_save1_diff    ;
wire  [31:0]  csr_save2_diff    ;
wire  [31:0]  csr_save3_diff    ;
wire  [31:0]  csr_tid_diff      ;
wire  [31:0]  csr_tcfg_diff     ;
wire  [31:0]  csr_tval_diff     ;
wire  [31:0]  csr_ticlr_diff    ;
wire  [31:0]  csr_llbctl_diff   ;
wire  [31:0]  csr_tlbrentry_diff;
wire  [31:0]  csr_dmw0_diff     ;
wire  [31:0]  csr_dmw1_diff     ;
wire  [13:0]  commit_csr_waddr;

CPU  CPU_inst (
    .clock(aclk),
    .reset(reset),
    .io_araddr(araddr),
    .io_arburst(arburst),
    .io_arid(arid),
    .io_arlen(arlen),
    .io_arready(arready),
    .io_arsize(arsize),
    .io_arvalid(arvalid),
    .io_awaddr(awaddr),
    .io_awburst(awburst),
    .io_awid(awid),
    .io_awlen(awlen),
    .io_awready(awready),
    .io_awsize(awsize),
    .io_awvalid(awvalid),
    .io_bid(bid),
    .io_bready(bready),
    .io_bresp(bresp),
    .io_bvalid(bvalid),
    .io_rdata(rdata),
    .io_rid(rid),
    .io_rlast(rlast),
    .io_rready(rready),
    .io_rresp(rresp),
    .io_rvalid(rvalid),
    .io_wdata(wdata),
    .io_wlast(wlast),
    .io_wready(wready),
    .io_wstrb(wstrb),
    .io_wvalid(wvalid),
    .io_commit_pc_0(debug0_wb_pc),
    .io_commit_pc_1(debug1_wb_pc),
    .io_commit_en_0(cmt_valid0),
    .io_commit_en_1(cmt_valid1),
    .io_commit_rd_0(debug0_wb_rf_wnum),
    .io_commit_rd_1(debug1_wb_rf_wnum),
    .io_commit_rf_wdata_0(debug0_wb_rf_wdata),
    .io_commit_rf_wdata_1(debug1_wb_rf_wdata),
    .io_commit_rd_valid_0(rf_wen_0),
    .io_commit_rd_valid_1(rf_wen_1),
    .io_commit_inst_0(cmt_inst0),
    .io_commit_inst_1(cmt_inst1),
    .io_commit_csr_waddr(commit_csr_waddr),
    .io_commit_cnt(cmt_stable_counter),
    .io_commit_csr_exception(cmt_exception),
    .io_diff_csr_crmd       (csr_crmd_diff     ),
    .io_diff_csr_prmd       (csr_prmd_diff     ),
    .io_diff_csr_euen       (csr_euen_diff     ),
    .io_diff_csr_ecfg       (csr_ectl_diff     ),
    .io_diff_csr_estat      (csr_estat_diff    ),
    .io_diff_csr_era        (csr_era_diff      ),
    .io_diff_csr_badv       (csr_badv_diff     ),
    .io_diff_csr_eentry     (csr_eentry_diff   ),
    .io_diff_csr_tlbidx     (csr_tlbidx_diff   ),
    .io_diff_csr_tlbehi     (csr_tlbehi_diff   ),
    .io_diff_csr_tlbelo0    (csr_tlbelo0_diff  ),
    .io_diff_csr_tlbelo1    (csr_tlbelo1_diff  ),
    .io_diff_csr_asid       (csr_asid_diff     ),
    .io_diff_csr_pgdl       (csr_pgdl_diff     ),
    .io_diff_csr_pgdh       (csr_pgdh_diff     ),
    .io_diff_csr_save0      (csr_save0_diff    ),
    .io_diff_csr_save1      (csr_save1_diff    ),
    .io_diff_csr_save2      (csr_save2_diff    ),
    .io_diff_csr_save3      (csr_save3_diff    ),
    .io_diff_csr_tid        (csr_tid_diff      ),
    .io_diff_csr_tcfg       (csr_tcfg_diff     ),
    .io_diff_csr_tval       (csr_tval_diff     ),
    .io_diff_csr_ticlr      (csr_ticlr_diff    ),
    .io_diff_csr_llbctl     (csr_llbctl_diff   ),
    .io_diff_csr_tlbrentry  (csr_tlbrentry_diff),
    .io_diff_csr_dmw0       (csr_dmw0_diff     ),
    .io_diff_csr_dmw1       (csr_dmw1_diff     )
);

wire inst_csrxchg0;
wire inst_csrxchg1;
wire inst_csrrd0;
wire inst_csrrd1;
wire inst_csrwr0;
wire inst_csrwr1;
assign inst_csrxchg0 = (cmt_inst0[31:24] == 'b00000100) & (~(cmt_inst0[9:5] == 'b00000) & ~(cmt_inst0[9:5] == 'b00001));
assign inst_csrxchg1 = (cmt_inst1[31:24] == 'b00000100) & (~(cmt_inst1[9:5] == 'b00000) & ~(cmt_inst1[9:5] == 'b00001));
assign inst_csrrd0 = (cmt_inst0[31:24] == 'b00000100) & (cmt_inst0[9:5] == 'b00000);
assign inst_csrrd1 = (cmt_inst1[31:24] == 'b00000100) & (cmt_inst1[9:5] == 'b00000);
assign inst_csrwr0 = (cmt_inst0[31:24] == 'b00000100) & (cmt_inst0[9:5] == 'b00001);
assign inst_csrwr1 = (cmt_inst1[31:24] == 'b00000100) & (cmt_inst1[9:5] == 'b00001);

/*DifftestInstrCommit DifftestInstrCommit0
    (
        .clock(aclk),
        .coreid(0),
        .index(0),
        .valid(cmt_valid0),
        .pc({32'd0,debug0_wb_pc}),
        .instr(cmt_inst0),
        .skip(0),
        .is_TLBFILL(0),
        .TLBFILL_index(0),
        .is_CNTinst(cmt_inst0[31:11]=='b000000000000000001100),
        .timer_64_value(cmt_stable_counter),
        .wen(rf_wen_0),
        .wdest({3'd0,debug0_wb_rf_wnum}),
        .wdata({32'd0,debug0_wb_rf_wdata}),
        .csr_rstat((inst_csrxchg0 || inst_csrrd0 || inst_csrwr0) && (commit_csr_waddr == 14'h5)),
        .csr_data(csr_estat_diff)
    );

DifftestInstrCommit DifftestInstrCommit1
    (
        .clock(aclk),
        .coreid(0),
        .index(1),
        .valid(cmt_valid1),
        .pc({32'd0,debug1_wb_pc}),
        .instr(cmt_inst1),
        .skip(0),
        .is_TLBFILL(0),
        .TLBFILL_index(0),
        .is_CNTinst(cmt_inst1[31:11]=='b000000000000000001100),
        .timer_64_value(cmt_stable_counter),
        .wen(rf_wen_1),
        .wdest({3'd0,debug1_wb_rf_wnum}),
        .wdata({32'd0,debug1_wb_rf_wdata}),
        .csr_rstat((inst_csrxchg1 || inst_csrrd1 || inst_csrwr1) && (commit_csr_waddr == 14'h5)),
        .csr_data(csr_estat_diff)
    );

DifftestExcpEvent DifftestExcpEvent
    (
        .clock(aclk),
        .coreid(0),
        .excp_valid(cmt_exception[7]),
        .eret(cmt_inst0=='b00000110010010000011100000000000),
        .intrNo(csr_estat_diff[12:2]),
        .cause(cmt_exception[5:0]),
        .exceptionPC({32'd0,debug0_wb_pc}),
        .exceptionInst(cmt_inst0)
    );

DifftestTrapEvent DifftestTrapEvent
    (
        .clock(aclk),
        .coreid(0),
        .valid(0),
        .code(0),
        .pc(0),
        .cycleCnt(0),
        .instrCnt(0)
    );

DifftestStoreEvent DifftestStoreEvent0
    (
        .clock(aclk),
        .coreid(0),
        .index(0),
        .valid(0),
        .storePAddr(0),
        .storeVAddr(0),
        .storeData(0)
    );

DifftestStoreEvent DifftestStoreEvent1
    (
        .clock(aclk),
        .coreid(0),
        .index(1),
        .valid(0),
        .storePAddr(0),
        .storeVAddr(0),
        .storeData(0)
    );

DifftestLoadEvent DifftestLoadEvent0
    (
        .clock(aclk),
        .coreid(0),
        .index(0),
        .valid(0),
        .paddr(0),
        .vaddr(0)
    );

DifftestLoadEvent DifftestLoadEvent1
    (
        .clock(aclk),
        .coreid(0),
        .index(1),
        .valid(0),
        .paddr(0),
        .vaddr(0)
    );

    DifftestGRegState DifftestGRegState(
        .clock              (aclk       ),
        .coreid             (0          ),
        .gpr_0              (0          ),
        .gpr_1              (logic_rf[1]    ),
        .gpr_2              (logic_rf[2]    ),
        .gpr_3              (logic_rf[3]    ),
        .gpr_4              (logic_rf[4]    ),
        .gpr_5              (logic_rf[5]    ),
        .gpr_6              (logic_rf[6]    ),
        .gpr_7              (logic_rf[7]    ),
        .gpr_8              (logic_rf[8]    ),
        .gpr_9              (logic_rf[9]    ),
        .gpr_10             (logic_rf[10]   ),
        .gpr_11             (logic_rf[11]   ),
        .gpr_12             (logic_rf[12]   ),
        .gpr_13             (logic_rf[13]   ),
        .gpr_14             (logic_rf[14]   ),
        .gpr_15             (logic_rf[15]   ),
        .gpr_16             (logic_rf[16]   ),
        .gpr_17             (logic_rf[17]   ),
        .gpr_18             (logic_rf[18]   ),
        .gpr_19             (logic_rf[19]   ),
        .gpr_20             (logic_rf[20]   ),
        .gpr_21             (logic_rf[21]   ),
        .gpr_22             (logic_rf[22]   ),
        .gpr_23             (logic_rf[23]   ),
        .gpr_24             (logic_rf[24]   ),
        .gpr_25             (logic_rf[25]   ),
        .gpr_26             (logic_rf[26]   ),
        .gpr_27             (logic_rf[27]   ),
        .gpr_28             (logic_rf[28]   ),
        .gpr_29             (logic_rf[29]   ),
        .gpr_30             (logic_rf[30]   ),
        .gpr_31             (logic_rf[31]   )
    );

     DifftestCSRRegState DifftestCSRRegState(
        .clock              (aclk             ),
        .coreid             (0                ),
        .crmd               (csr_crmd_diff    ),
        .prmd               (csr_prmd_diff    ),
        .euen               (csr_euen_diff    ),
        .ecfg               (csr_ectl_diff    ),
        .estat              (csr_estat_diff   ),
        .era                (csr_era_diff     ),
        .badv               (csr_badv_diff    ),
        .eentry             (csr_eentry_diff  ),
        .tlbidx             (csr_tlbidx_diff  ),
        .tlbehi             (csr_tlbehi_diff  ),
        .tlbelo0            (csr_tlbelo0_diff ),
        .tlbelo1            (csr_tlbelo1_diff ),
        .asid               (csr_asid_diff    ),
        .pgdl               (csr_pgdl_diff    ),
        .pgdh               (csr_pgdh_diff    ),
        .save0              (csr_save0_diff   ),
        .save1              (csr_save1_diff   ),
        .save2              (csr_save2_diff   ),
        .save3              (csr_save3_diff   ),
        .tid                (csr_tid_diff     ),
        .tcfg               (csr_tcfg_diff    ),
        .tval               (csr_tval_diff    ),
        .ticlr              (csr_ticlr_diff   ),
        .llbctl             (csr_llbctl_diff  ),
        .tlbrentry          (csr_tlbrentry_diff),
        .dmw0               (csr_dmw0_diff    ),
        .dmw1               (csr_dmw1_diff    )
    );
*/    
    LogicRegFile LogicRegFile(
        .aclk(aclk),
        .reset(reset),
        .cmt_valid0(cmt_valid0),
        .cmt_valid1(cmt_valid1),
        .rf_wen_0(rf_wen_0),
        .rf_wen_1(rf_wen_1),
        .debug0_wb_rf_wdata(debug0_wb_rf_wdata),
        .debug1_wb_rf_wdata(debug1_wb_rf_wdata),
        .debug0_wb_rf_wnum(debug0_wb_rf_wnum),
        .debug1_wb_rf_wnum(debug1_wb_rf_wnum),
        .logic_rf(logic_rf)
    );

endmodule


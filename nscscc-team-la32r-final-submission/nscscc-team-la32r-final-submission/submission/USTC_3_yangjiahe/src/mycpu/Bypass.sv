// Generated by CIRCT firtool-1.62.0
module Bypass(
  input         io_rd_valid_wb_0,
                io_rd_valid_wb_1,
                io_rd_valid_wb_2,
  input  [5:0]  io_prd_wb_0,
                io_prd_wb_1,
                io_prd_wb_2,
  input  [31:0] io_prd_wdata_wb_0,
                io_prd_wdata_wb_1,
                io_prd_wdata_wb_2,
  input  [5:0]  io_prj_ex_0,
                io_prj_ex_1,
                io_prj_ex_2,
                io_prj_ex_3,
                io_prk_ex_0,
                io_prk_ex_1,
                io_prk_ex_2,
                io_prk_ex_3,
  output        io_forward_prj_en_0,
                io_forward_prj_en_1,
                io_forward_prj_en_2,
                io_forward_prj_en_3,
                io_forward_prk_en_0,
                io_forward_prk_en_1,
                io_forward_prk_en_2,
                io_forward_prk_en_3,
  output [31:0] io_forward_prj_data_0,
                io_forward_prj_data_1,
                io_forward_prj_data_2,
                io_forward_prj_data_3,
                io_forward_prk_data_0,
                io_forward_prk_data_1,
                io_forward_prk_data_2,
                io_forward_prk_data_3
);

  wire        _io_forward_prj_data_0_T = io_prd_wb_0 == io_prj_ex_0;
  wire        _io_forward_prk_data_0_T = io_prd_wb_0 == io_prk_ex_0;
  wire        _io_forward_prj_data_1_T = io_prd_wb_0 == io_prj_ex_1;
  wire        _io_forward_prk_data_1_T = io_prd_wb_0 == io_prk_ex_1;
  wire        _io_forward_prj_data_2_T = io_prd_wb_0 == io_prj_ex_2;
  wire        _io_forward_prk_data_2_T = io_prd_wb_0 == io_prk_ex_2;
  wire        hit_j_3_0 = io_rd_valid_wb_0 & io_prd_wb_0 == io_prj_ex_3 & (|io_prd_wb_0);
  wire        hit_j_3_1 = io_rd_valid_wb_1 & io_prd_wb_1 == io_prj_ex_3 & (|io_prd_wb_1);
  wire        hit_j_3_2 = io_rd_valid_wb_2 & io_prd_wb_2 == io_prj_ex_3 & (|io_prd_wb_2);
  wire [31:0] _io_forward_prj_data_3_T = hit_j_3_0 ? io_prd_wdata_wb_0 : 32'h0;
  wire [31:0] _io_forward_prj_data_3_T_1 = hit_j_3_1 ? io_prd_wdata_wb_1 : 32'h0;
  wire [31:0] _io_forward_prj_data_3_T_2 = hit_j_3_2 ? io_prd_wdata_wb_2 : 32'h0;
  wire        hit_k_3_0 = io_rd_valid_wb_0 & io_prd_wb_0 == io_prk_ex_3 & (|io_prd_wb_0);
  wire        hit_k_3_1 = io_rd_valid_wb_1 & io_prd_wb_1 == io_prk_ex_3 & (|io_prd_wb_1);
  wire        hit_k_3_2 = io_rd_valid_wb_2 & io_prd_wb_2 == io_prk_ex_3 & (|io_prd_wb_2);
  wire [31:0] _io_forward_prk_data_3_T = hit_k_3_0 ? io_prd_wdata_wb_0 : 32'h0;
  wire [31:0] _io_forward_prk_data_3_T_1 = hit_k_3_1 ? io_prd_wdata_wb_1 : 32'h0;
  wire [31:0] _io_forward_prk_data_3_T_2 = hit_k_3_2 ? io_prd_wdata_wb_2 : 32'h0;
  assign io_forward_prj_en_0 =
    |{io_rd_valid_wb_1 & io_prd_wb_1 == io_prj_ex_0 & (|io_prd_wb_1),
      io_rd_valid_wb_0 & _io_forward_prj_data_0_T & (|io_prd_wb_0)};
  assign io_forward_prj_en_1 =
    |{io_rd_valid_wb_1 & io_prd_wb_1 == io_prj_ex_1 & (|io_prd_wb_1),
      io_rd_valid_wb_0 & _io_forward_prj_data_1_T & (|io_prd_wb_0)};
  assign io_forward_prj_en_2 =
    |{io_rd_valid_wb_1 & io_prd_wb_1 == io_prj_ex_2 & (|io_prd_wb_1),
      io_rd_valid_wb_0 & _io_forward_prj_data_2_T & (|io_prd_wb_0)};
  assign io_forward_prj_en_3 = |{hit_j_3_2, hit_j_3_1, hit_j_3_0};
  assign io_forward_prk_en_0 =
    |{io_rd_valid_wb_1 & io_prd_wb_1 == io_prk_ex_0 & (|io_prd_wb_1),
      io_rd_valid_wb_0 & _io_forward_prk_data_0_T & (|io_prd_wb_0)};
  assign io_forward_prk_en_1 =
    |{io_rd_valid_wb_1 & io_prd_wb_1 == io_prk_ex_1 & (|io_prd_wb_1),
      io_rd_valid_wb_0 & _io_forward_prk_data_1_T & (|io_prd_wb_0)};
  assign io_forward_prk_en_2 =
    |{io_rd_valid_wb_1 & io_prd_wb_1 == io_prk_ex_2 & (|io_prd_wb_1),
      io_rd_valid_wb_0 & _io_forward_prk_data_2_T & (|io_prd_wb_0)};
  assign io_forward_prk_en_3 = |{hit_k_3_2, hit_k_3_1, hit_k_3_0};
  assign io_forward_prj_data_0 =
    _io_forward_prj_data_0_T ? io_prd_wdata_wb_0 : io_prd_wdata_wb_1;
  assign io_forward_prj_data_1 =
    _io_forward_prj_data_1_T ? io_prd_wdata_wb_0 : io_prd_wdata_wb_1;
  assign io_forward_prj_data_2 =
    _io_forward_prj_data_2_T ? io_prd_wdata_wb_0 : io_prd_wdata_wb_1;
  assign io_forward_prj_data_3 =
    _io_forward_prj_data_3_T | _io_forward_prj_data_3_T_1 | _io_forward_prj_data_3_T_2;
  assign io_forward_prk_data_0 =
    _io_forward_prk_data_0_T ? io_prd_wdata_wb_0 : io_prd_wdata_wb_1;
  assign io_forward_prk_data_1 =
    _io_forward_prk_data_1_T ? io_prd_wdata_wb_0 : io_prd_wdata_wb_1;
  assign io_forward_prk_data_2 =
    _io_forward_prk_data_2_T ? io_prd_wdata_wb_0 : io_prd_wdata_wb_1;
  assign io_forward_prk_data_3 =
    _io_forward_prk_data_3_T | _io_forward_prk_data_3_T_1 | _io_forward_prk_data_3_T_2;
endmodule


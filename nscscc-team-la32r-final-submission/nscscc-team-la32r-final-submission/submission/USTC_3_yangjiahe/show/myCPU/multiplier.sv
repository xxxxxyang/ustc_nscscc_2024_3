// Generated by CIRCT firtool-1.62.0
module multiplier(
  input         clock,
                reset,
  input  [31:0] io_src1,
                io_src2,
  output [31:0] io_res,
  input  [3:0]  io_op,
  input         io_busy
);

  reg  [65:0]      booth_reg_0;
  reg  [65:0]      booth_reg_1;
  reg  [65:0]      booth_reg_2;
  reg  [65:0]      booth_reg_3;
  reg  [65:0]      booth_reg_4;
  reg  [65:0]      booth_reg_5;
  reg  [65:0]      booth_reg_6;
  reg  [65:0]      booth_reg_7;
  reg  [65:0]      booth_reg_8;
  reg  [65:0]      booth_reg_9;
  reg  [65:0]      booth_reg_10;
  reg  [65:0]      booth_reg_11;
  reg  [65:0]      booth_reg_12;
  reg  [65:0]      booth_reg_13;
  reg  [65:0]      booth_reg_14;
  reg  [65:0]      booth_reg_15;
  reg  [65:0]      booth_reg_16;
  reg  [3:0]       op_stage1;
  reg  [65:0]      tree_temp_0;
  reg  [65:0]      tree_temp_1;
  reg  [3:0]       op_stage2;
  wire [63:0]      _res_temp_T = 64'(tree_temp_0[63:0] + tree_temp_1[63:0]);
  wire [33:0]      _src1_T_3 = io_op[1] ? 34'h0 : {34{io_src1[31]}};
  wire [65:0]      src1 = {_src1_T_3, io_src1};
  wire [65:0]      _booth_res_T_13 = {_src1_T_3[31:0], io_src1, 2'h0};
  wire [65:0]      _GEN = {~(_src1_T_3[31:0]), ~io_src1, 2'h3};
  wire [65:0]      _booth_res_T_24 = {_src1_T_3[29:0], io_src1, 4'h0};
  wire [65:0]      _GEN_0 = {~(_src1_T_3[29:0]), ~io_src1, 4'hF};
  wire [65:0]      _booth_res_T_35 = {_src1_T_3[27:0], io_src1, 6'h0};
  wire [65:0]      _GEN_1 = {~(_src1_T_3[27:0]), ~io_src1, 6'h3F};
  wire [65:0]      _booth_res_T_46 = {_src1_T_3[25:0], io_src1, 8'h0};
  wire [65:0]      _GEN_2 = {~(_src1_T_3[25:0]), ~io_src1, 8'hFF};
  wire [65:0]      _booth_res_T_57 = {_src1_T_3[23:0], io_src1, 10'h0};
  wire [65:0]      _GEN_3 = {~(_src1_T_3[23:0]), ~io_src1, 10'h3FF};
  wire [65:0]      _booth_res_T_68 = {_src1_T_3[21:0], io_src1, 12'h0};
  wire [65:0]      _GEN_4 = {~(_src1_T_3[21:0]), ~io_src1, 12'hFFF};
  wire [65:0]      _booth_res_T_79 = {_src1_T_3[19:0], io_src1, 14'h0};
  wire [65:0]      _GEN_5 = {~(_src1_T_3[19:0]), ~io_src1, 14'h3FFF};
  wire [65:0]      _booth_res_T_90 = {_src1_T_3[17:0], io_src1, 16'h0};
  wire [65:0]      _GEN_6 = {~(_src1_T_3[17:0]), ~io_src1, 16'hFFFF};
  wire [65:0]      _booth_res_T_101 = {_src1_T_3[15:0], io_src1, 18'h0};
  wire [65:0]      _GEN_7 = {~(_src1_T_3[15:0]), ~io_src1, 18'h3FFFF};
  wire [65:0]      _booth_res_T_112 = {_src1_T_3[13:0], io_src1, 20'h0};
  wire [65:0]      _GEN_8 = {~(_src1_T_3[13:0]), ~io_src1, 20'hFFFFF};
  wire [65:0]      _booth_res_T_123 = {_src1_T_3[11:0], io_src1, 22'h0};
  wire [65:0]      _GEN_9 = {~(_src1_T_3[11:0]), ~io_src1, 22'h3FFFFF};
  wire [65:0]      _booth_res_T_134 = {_src1_T_3[9:0], io_src1, 24'h0};
  wire [65:0]      _GEN_10 = {~(_src1_T_3[9:0]), ~io_src1, 24'hFFFFFF};
  wire [65:0]      _booth_res_T_145 = {_src1_T_3[7:0], io_src1, 26'h0};
  wire [65:0]      _GEN_11 = {~(_src1_T_3[7:0]), ~io_src1, 26'h3FFFFFF};
  wire [65:0]      _booth_res_T_156 = {_src1_T_3[5:0], io_src1, 28'h0};
  wire [65:0]      _GEN_12 = {~(_src1_T_3[5:0]), ~io_src1, 28'hFFFFFFF};
  wire [65:0]      _booth_res_T_167 = {_src1_T_3[3:0], io_src1, 30'h0};
  wire [65:0]      _GEN_13 = {~(_src1_T_3[3:0]), ~io_src1, 30'h3FFFFFFF};
  wire [65:0]      _booth_res_T_178 = {_src1_T_3[1:0], io_src1, 32'h0};
  wire [65:0]      _GEN_14 = {~(_src1_T_3[1:0]), ~io_src1, 32'hFFFFFFFF};
  wire [65:0]      _t1_res2_T = booth_reg_0 ^ booth_reg_1;
  wire [65:0]      t1_res1 = _t1_res2_T ^ booth_reg_2;
  wire [64:0]      _t1_res2_T_3 =
    _t1_res2_T[64:0] & booth_reg_2[64:0] | booth_reg_0[64:0] & booth_reg_1[64:0];
  wire [65:0]      _t1_res2_T_6 = booth_reg_3 ^ booth_reg_4;
  wire [65:0]      t1_res1_1 = _t1_res2_T_6 ^ booth_reg_5;
  wire [64:0]      _t1_res2_T_9 =
    _t1_res2_T_6[64:0] & booth_reg_5[64:0] | booth_reg_3[64:0] & booth_reg_4[64:0];
  wire [65:0]      _t1_res2_T_12 = booth_reg_6 ^ booth_reg_7;
  wire [65:0]      t1_res1_2 = _t1_res2_T_12 ^ booth_reg_8;
  wire [64:0]      _t1_res2_T_15 =
    _t1_res2_T_12[64:0] & booth_reg_8[64:0] | booth_reg_6[64:0] & booth_reg_7[64:0];
  wire [65:0]      _t1_res2_T_18 = booth_reg_9 ^ booth_reg_10;
  wire [65:0]      t1_res1_3 = _t1_res2_T_18 ^ booth_reg_11;
  wire [64:0]      _t1_res2_T_21 =
    _t1_res2_T_18[64:0] & booth_reg_11[64:0] | booth_reg_9[64:0] & booth_reg_10[64:0];
  wire [65:0]      _t1_res2_T_24 = booth_reg_12 ^ booth_reg_13;
  wire [65:0]      t1_res1_4 = _t1_res2_T_24 ^ booth_reg_14;
  wire [64:0]      _t1_res2_T_27 =
    _t1_res2_T_24[64:0] & booth_reg_14[64:0] | booth_reg_12[64:0] & booth_reg_13[64:0];
  wire [65:0]      _t2_res2_T = t1_res1 ^ {_t1_res2_T_3, 1'h0};
  wire [65:0]      t2_res1 = _t2_res2_T ^ t1_res1_1;
  wire [64:0]      _t2_res2_T_3 =
    _t2_res2_T[64:0] & t1_res1_1[64:0] | t1_res1[64:0] & {_t1_res2_T_3[63:0], 1'h0};
  wire [65:0]      _t2_res2_T_6 = {_t1_res2_T_9, 1'h0} ^ t1_res1_2;
  wire [65:0]      t2_res1_1 = _t2_res2_T_6 ^ {_t1_res2_T_15, 1'h0};
  wire [64:0]      _t2_res2_T_9 =
    _t2_res2_T_6[64:0] & {_t1_res2_T_15[63:0], 1'h0} | {_t1_res2_T_9[63:0], 1'h0}
    & t1_res1_2[64:0];
  wire [65:0]      _t2_res2_T_12 = t1_res1_3 ^ {_t1_res2_T_21, 1'h0};
  wire [65:0]      t2_res1_2 = _t2_res2_T_12 ^ t1_res1_4;
  wire [64:0]      _t2_res2_T_15 =
    _t2_res2_T_12[64:0] & t1_res1_4[64:0] | t1_res1_3[64:0] & {_t1_res2_T_21[63:0], 1'h0};
  wire [65:0]      _t2_res2_T_18 = {_t1_res2_T_27, 1'h0} ^ booth_reg_15;
  wire [65:0]      temp3_4 = _t2_res2_T_18 ^ booth_reg_16;
  wire [64:0]      _t2_res2_T_21 =
    _t2_res2_T_18[64:0] & booth_reg_16[64:0] | {_t1_res2_T_27[63:0], 1'h0}
    & booth_reg_15[64:0];
  wire [65:0]      _t3_res2_T = t2_res1 ^ {_t2_res2_T_3, 1'h0};
  wire [65:0]      t3_res1 = _t3_res2_T ^ t2_res1_1;
  wire [64:0]      _t3_res2_T_3 =
    _t3_res2_T[64:0] & t2_res1_1[64:0] | t2_res1[64:0] & {_t2_res2_T_3[63:0], 1'h0};
  wire [65:0]      _t3_res2_T_6 = {_t2_res2_T_9, 1'h0} ^ t2_res1_2;
  wire [65:0]      t3_res1_1 = _t3_res2_T_6 ^ {_t2_res2_T_15, 1'h0};
  wire [64:0]      _t3_res2_T_9 =
    _t3_res2_T_6[64:0] & {_t2_res2_T_15[63:0], 1'h0} | {_t2_res2_T_9[63:0], 1'h0}
    & t2_res1_2[64:0];
  wire [65:0]      _t4_res2_T = t3_res1 ^ {_t3_res2_T_3, 1'h0};
  wire [65:0]      t4_res1 = _t4_res2_T ^ t3_res1_1;
  wire [64:0]      _t4_res2_T_3 =
    _t4_res2_T[64:0] & t3_res1_1[64:0] | t3_res1[64:0] & {_t3_res2_T_3[63:0], 1'h0};
  wire [65:0]      _t4_res2_T_6 = {_t3_res2_T_9, 1'h0} ^ temp3_4;
  wire [65:0]      t4_res1_1 = _t4_res2_T_6 ^ {_t2_res2_T_21, 1'h0};
  wire [64:0]      _t4_res2_T_9 =
    _t4_res2_T_6[64:0] & {_t2_res2_T_21[63:0], 1'h0} | {_t3_res2_T_9[63:0], 1'h0}
    & temp3_4[64:0];
  wire [65:0]      _t5_res2_T = t4_res1 ^ {_t4_res2_T_3, 1'h0};
  wire [65:0]      t5_res1 = _t5_res2_T ^ t4_res1_1;
  wire [64:0]      _t5_res2_T_3 =
    _t5_res2_T[64:0] & t4_res1_1[64:0] | t4_res1[64:0] & {_t4_res2_T_3[63:0], 1'h0};
  wire [65:0]      _t6_res2_T = t5_res1 ^ {_t5_res2_T_3, 1'h0};
  wire [3:0][65:0] _GEN_15 =
    {{66'(~src1 + 66'h1)},
     {66'({~(_src1_T_3[32:0]), ~io_src1, 1'h1} + 66'h1)},
     {src1},
     {66'h0}};
  wire [7:0][65:0] _GEN_16 =
    {{66'h0},
     {66'(_GEN + 66'h1)},
     {66'(_GEN + 66'h1)},
     {66'({~(_src1_T_3[30:0]), ~io_src1, 3'h7} + 66'h1)},
     {{_src1_T_3[30:0], io_src1, 3'h0}},
     {_booth_res_T_13},
     {_booth_res_T_13},
     {66'h0}};
  wire [7:0][65:0] _GEN_17 =
    {{66'h0},
     {66'(_GEN_0 + 66'h1)},
     {66'(_GEN_0 + 66'h1)},
     {66'({~(_src1_T_3[28:0]), ~io_src1, 5'h1F} + 66'h1)},
     {{_src1_T_3[28:0], io_src1, 5'h0}},
     {_booth_res_T_24},
     {_booth_res_T_24},
     {66'h0}};
  wire [7:0][65:0] _GEN_18 =
    {{66'h0},
     {66'(_GEN_1 + 66'h1)},
     {66'(_GEN_1 + 66'h1)},
     {66'({~(_src1_T_3[26:0]), ~io_src1, 7'h7F} + 66'h1)},
     {{_src1_T_3[26:0], io_src1, 7'h0}},
     {_booth_res_T_35},
     {_booth_res_T_35},
     {66'h0}};
  wire [7:0][65:0] _GEN_19 =
    {{66'h0},
     {66'(_GEN_2 + 66'h1)},
     {66'(_GEN_2 + 66'h1)},
     {66'({~(_src1_T_3[24:0]), ~io_src1, 9'h1FF} + 66'h1)},
     {{_src1_T_3[24:0], io_src1, 9'h0}},
     {_booth_res_T_46},
     {_booth_res_T_46},
     {66'h0}};
  wire [7:0][65:0] _GEN_20 =
    {{66'h0},
     {66'(_GEN_3 + 66'h1)},
     {66'(_GEN_3 + 66'h1)},
     {66'({~(_src1_T_3[22:0]), ~io_src1, 11'h7FF} + 66'h1)},
     {{_src1_T_3[22:0], io_src1, 11'h0}},
     {_booth_res_T_57},
     {_booth_res_T_57},
     {66'h0}};
  wire [7:0][65:0] _GEN_21 =
    {{66'h0},
     {66'(_GEN_4 + 66'h1)},
     {66'(_GEN_4 + 66'h1)},
     {66'({~(_src1_T_3[20:0]), ~io_src1, 13'h1FFF} + 66'h1)},
     {{_src1_T_3[20:0], io_src1, 13'h0}},
     {_booth_res_T_68},
     {_booth_res_T_68},
     {66'h0}};
  wire [7:0][65:0] _GEN_22 =
    {{66'h0},
     {66'(_GEN_5 + 66'h1)},
     {66'(_GEN_5 + 66'h1)},
     {66'({~(_src1_T_3[18:0]), ~io_src1, 15'h7FFF} + 66'h1)},
     {{_src1_T_3[18:0], io_src1, 15'h0}},
     {_booth_res_T_79},
     {_booth_res_T_79},
     {66'h0}};
  wire [7:0][65:0] _GEN_23 =
    {{66'h0},
     {66'(_GEN_6 + 66'h1)},
     {66'(_GEN_6 + 66'h1)},
     {66'({~(_src1_T_3[16:0]), ~io_src1, 17'h1FFFF} + 66'h1)},
     {{_src1_T_3[16:0], io_src1, 17'h0}},
     {_booth_res_T_90},
     {_booth_res_T_90},
     {66'h0}};
  wire [7:0][65:0] _GEN_24 =
    {{66'h0},
     {66'(_GEN_7 + 66'h1)},
     {66'(_GEN_7 + 66'h1)},
     {66'({~(_src1_T_3[14:0]), ~io_src1, 19'h7FFFF} + 66'h1)},
     {{_src1_T_3[14:0], io_src1, 19'h0}},
     {_booth_res_T_101},
     {_booth_res_T_101},
     {66'h0}};
  wire [7:0][65:0] _GEN_25 =
    {{66'h0},
     {66'(_GEN_8 + 66'h1)},
     {66'(_GEN_8 + 66'h1)},
     {66'({~(_src1_T_3[12:0]), ~io_src1, 21'h1FFFFF} + 66'h1)},
     {{_src1_T_3[12:0], io_src1, 21'h0}},
     {_booth_res_T_112},
     {_booth_res_T_112},
     {66'h0}};
  wire [7:0][65:0] _GEN_26 =
    {{66'h0},
     {66'(_GEN_9 + 66'h1)},
     {66'(_GEN_9 + 66'h1)},
     {66'({~(_src1_T_3[10:0]), ~io_src1, 23'h7FFFFF} + 66'h1)},
     {{_src1_T_3[10:0], io_src1, 23'h0}},
     {_booth_res_T_123},
     {_booth_res_T_123},
     {66'h0}};
  wire [7:0][65:0] _GEN_27 =
    {{66'h0},
     {66'(_GEN_10 + 66'h1)},
     {66'(_GEN_10 + 66'h1)},
     {66'({~(_src1_T_3[8:0]), ~io_src1, 25'h1FFFFFF} + 66'h1)},
     {{_src1_T_3[8:0], io_src1, 25'h0}},
     {_booth_res_T_134},
     {_booth_res_T_134},
     {66'h0}};
  wire [7:0][65:0] _GEN_28 =
    {{66'h0},
     {66'(_GEN_11 + 66'h1)},
     {66'(_GEN_11 + 66'h1)},
     {66'({~(_src1_T_3[6:0]), ~io_src1, 27'h7FFFFFF} + 66'h1)},
     {{_src1_T_3[6:0], io_src1, 27'h0}},
     {_booth_res_T_145},
     {_booth_res_T_145},
     {66'h0}};
  wire [7:0][65:0] _GEN_29 =
    {{66'h0},
     {66'(_GEN_12 + 66'h1)},
     {66'(_GEN_12 + 66'h1)},
     {66'({~(_src1_T_3[4:0]), ~io_src1, 29'h1FFFFFFF} + 66'h1)},
     {{_src1_T_3[4:0], io_src1, 29'h0}},
     {_booth_res_T_156},
     {_booth_res_T_156},
     {66'h0}};
  wire [7:0][65:0] _GEN_30 =
    {{66'h0},
     {66'(_GEN_13 + 66'h1)},
     {66'(_GEN_13 + 66'h1)},
     {66'({~(_src1_T_3[2:0]), ~io_src1, 31'h7FFFFFFF} + 66'h1)},
     {{_src1_T_3[2:0], io_src1, 31'h0}},
     {_booth_res_T_167},
     {_booth_res_T_167},
     {66'h0}};
  wire [7:0][65:0] _GEN_31 =
    {{66'h0},
     {66'(_GEN_14 + 66'h1)},
     {66'(_GEN_14 + 66'h1)},
     {66'({~(_src1_T_3[0]), ~io_src1, 33'h1FFFFFFFF} + 66'h1)},
     {{_src1_T_3[0], io_src1, 33'h0}},
     {_booth_res_T_178},
     {_booth_res_T_178},
     {66'h0}};
  wire [1:0]       _src2_T_3 = io_op[1] ? 2'h0 : {2{io_src2[31]}};
  always @(posedge clock) begin
    if (reset) begin
      booth_reg_0 <= 66'h0;
      booth_reg_1 <= 66'h0;
      booth_reg_2 <= 66'h0;
      booth_reg_3 <= 66'h0;
      booth_reg_4 <= 66'h0;
      booth_reg_5 <= 66'h0;
      booth_reg_6 <= 66'h0;
      booth_reg_7 <= 66'h0;
      booth_reg_8 <= 66'h0;
      booth_reg_9 <= 66'h0;
      booth_reg_10 <= 66'h0;
      booth_reg_11 <= 66'h0;
      booth_reg_12 <= 66'h0;
      booth_reg_13 <= 66'h0;
      booth_reg_14 <= 66'h0;
      booth_reg_15 <= 66'h0;
      booth_reg_16 <= 66'h0;
      op_stage1 <= 4'h0;
      tree_temp_0 <= 66'h0;
      tree_temp_1 <= 66'h0;
      op_stage2 <= 4'h0;
    end
    else if (io_busy) begin
    end
    else begin
      booth_reg_0 <= _GEN_15[io_src2[1:0]];
      booth_reg_1 <= _GEN_16[io_src2[3:1]];
      booth_reg_2 <= _GEN_17[io_src2[5:3]];
      booth_reg_3 <= _GEN_18[io_src2[7:5]];
      booth_reg_4 <= _GEN_19[io_src2[9:7]];
      booth_reg_5 <= _GEN_20[io_src2[11:9]];
      booth_reg_6 <= _GEN_21[io_src2[13:11]];
      booth_reg_7 <= _GEN_22[io_src2[15:13]];
      booth_reg_8 <= _GEN_23[io_src2[17:15]];
      booth_reg_9 <= _GEN_24[io_src2[19:17]];
      booth_reg_10 <= _GEN_25[io_src2[21:19]];
      booth_reg_11 <= _GEN_26[io_src2[23:21]];
      booth_reg_12 <= _GEN_27[io_src2[25:23]];
      booth_reg_13 <= _GEN_28[io_src2[27:25]];
      booth_reg_14 <= _GEN_29[io_src2[29:27]];
      booth_reg_15 <= _GEN_30[io_src2[31:29]];
      booth_reg_16 <= _GEN_31[{_src2_T_3, io_src2[31]}];
      op_stage1 <= io_op;
      tree_temp_0 <= _t6_res2_T ^ {_t4_res2_T_9, 1'h0};
      tree_temp_1 <=
        {_t6_res2_T[64:0] & {_t4_res2_T_9[63:0], 1'h0} | t5_res1[64:0]
           & {_t5_res2_T_3[63:0], 1'h0},
         1'h0};
      op_stage2 <= op_stage1;
    end
  end // always @(posedge)
  assign io_res = op_stage2 == 4'h0 ? _res_temp_T[31:0] : _res_temp_T[63:32];
endmodule


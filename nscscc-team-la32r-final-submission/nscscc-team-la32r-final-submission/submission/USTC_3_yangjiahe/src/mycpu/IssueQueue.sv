// Generated by CIRCT firtool-1.62.0
module IssueQueue(
  input         clock,
                reset,
                io_insts_inst_valid_0,
                io_insts_inst_valid_1,
                io_insts_rd_valid_0,
                io_insts_rd_valid_1,
  input  [31:0] io_insts_imm_0,
                io_insts_imm_1,
  input  [3:0]  io_insts_alu_op_0,
                io_insts_alu_op_1,
  input  [4:0]  io_insts_mem_type_0,
                io_insts_mem_type_1,
  input  [12:0] io_insts_priv_vec_0,
                io_insts_priv_vec_1,
  input  [5:0]  io_insts_prj_0,
                io_insts_prj_1,
                io_insts_prk_0,
                io_insts_prk_1,
                io_insts_prd_0,
                io_insts_prd_1,
  input  [4:0]  io_insts_rob_index_0,
                io_insts_rob_index_1,
  input         io_insts_valid_0,
                io_insts_valid_1,
                io_prj_ready_0,
                io_prj_ready_1,
                io_prk_ready_0,
                io_prk_ready_1,
  input  [5:0]  io_wake_preg_0,
                io_wake_preg_1,
                io_wake_preg_2,
                io_wake_preg_3,
  output        io_issue_inst_inst_valid,
                io_issue_inst_rd_valid,
  output [31:0] io_issue_inst_imm,
  output [3:0]  io_issue_inst_alu_op,
  output [4:0]  io_issue_inst_mem_type,
  output [12:0] io_issue_inst_priv_vec,
  output [5:0]  io_issue_inst_prj,
                io_issue_inst_prk,
                io_issue_inst_prd,
  output [4:0]  io_issue_inst_rob_index,
  output        io_issue_valid,
  input         io_stall,
                io_stall_in,
                io_flush,
  output        io_full
);

  reg         queue_inst_inst_valid_0;
  reg         queue_inst_inst_valid_1;
  reg         queue_inst_inst_valid_2;
  reg         queue_inst_inst_valid_3;
  reg         queue_inst_inst_valid_4;
  reg         queue_inst_inst_valid_5;
  reg         queue_inst_inst_valid_6;
  reg         queue_inst_inst_valid_7;
  reg         queue_inst_rd_valid_0;
  reg         queue_inst_rd_valid_1;
  reg         queue_inst_rd_valid_2;
  reg         queue_inst_rd_valid_3;
  reg         queue_inst_rd_valid_4;
  reg         queue_inst_rd_valid_5;
  reg         queue_inst_rd_valid_6;
  reg         queue_inst_rd_valid_7;
  reg  [31:0] queue_inst_imm_0;
  reg  [31:0] queue_inst_imm_1;
  reg  [31:0] queue_inst_imm_2;
  reg  [31:0] queue_inst_imm_3;
  reg  [31:0] queue_inst_imm_4;
  reg  [31:0] queue_inst_imm_5;
  reg  [31:0] queue_inst_imm_6;
  reg  [31:0] queue_inst_imm_7;
  reg  [3:0]  queue_inst_alu_op_0;
  reg  [3:0]  queue_inst_alu_op_1;
  reg  [3:0]  queue_inst_alu_op_2;
  reg  [3:0]  queue_inst_alu_op_3;
  reg  [3:0]  queue_inst_alu_op_4;
  reg  [3:0]  queue_inst_alu_op_5;
  reg  [3:0]  queue_inst_alu_op_6;
  reg  [3:0]  queue_inst_alu_op_7;
  reg  [4:0]  queue_inst_mem_type_0;
  reg  [4:0]  queue_inst_mem_type_1;
  reg  [4:0]  queue_inst_mem_type_2;
  reg  [4:0]  queue_inst_mem_type_3;
  reg  [4:0]  queue_inst_mem_type_4;
  reg  [4:0]  queue_inst_mem_type_5;
  reg  [4:0]  queue_inst_mem_type_6;
  reg  [4:0]  queue_inst_mem_type_7;
  reg  [12:0] queue_inst_priv_vec_0;
  reg  [12:0] queue_inst_priv_vec_1;
  reg  [12:0] queue_inst_priv_vec_2;
  reg  [12:0] queue_inst_priv_vec_3;
  reg  [12:0] queue_inst_priv_vec_4;
  reg  [12:0] queue_inst_priv_vec_5;
  reg  [12:0] queue_inst_priv_vec_6;
  reg  [12:0] queue_inst_priv_vec_7;
  reg  [5:0]  queue_inst_prj_0;
  reg  [5:0]  queue_inst_prj_1;
  reg  [5:0]  queue_inst_prj_2;
  reg  [5:0]  queue_inst_prj_3;
  reg  [5:0]  queue_inst_prj_4;
  reg  [5:0]  queue_inst_prj_5;
  reg  [5:0]  queue_inst_prj_6;
  reg  [5:0]  queue_inst_prj_7;
  reg  [5:0]  queue_inst_prk_0;
  reg  [5:0]  queue_inst_prk_1;
  reg  [5:0]  queue_inst_prk_2;
  reg  [5:0]  queue_inst_prk_3;
  reg  [5:0]  queue_inst_prk_4;
  reg  [5:0]  queue_inst_prk_5;
  reg  [5:0]  queue_inst_prk_6;
  reg  [5:0]  queue_inst_prk_7;
  reg  [5:0]  queue_inst_prd_0;
  reg  [5:0]  queue_inst_prd_1;
  reg  [5:0]  queue_inst_prd_2;
  reg  [5:0]  queue_inst_prd_3;
  reg  [5:0]  queue_inst_prd_4;
  reg  [5:0]  queue_inst_prd_5;
  reg  [5:0]  queue_inst_prd_6;
  reg  [5:0]  queue_inst_prd_7;
  reg  [4:0]  queue_inst_rob_index_0;
  reg  [4:0]  queue_inst_rob_index_1;
  reg  [4:0]  queue_inst_rob_index_2;
  reg  [4:0]  queue_inst_rob_index_3;
  reg  [4:0]  queue_inst_rob_index_4;
  reg  [4:0]  queue_inst_rob_index_5;
  reg  [4:0]  queue_inst_rob_index_6;
  reg  [4:0]  queue_inst_rob_index_7;
  reg         queue_prj_ready_0;
  reg         queue_prj_ready_1;
  reg         queue_prj_ready_2;
  reg         queue_prj_ready_3;
  reg         queue_prj_ready_4;
  reg         queue_prj_ready_5;
  reg         queue_prj_ready_6;
  reg         queue_prj_ready_7;
  reg         queue_prk_ready_0;
  reg         queue_prk_ready_1;
  reg         queue_prk_ready_2;
  reg         queue_prk_ready_3;
  reg         queue_prk_ready_4;
  reg         queue_prk_ready_5;
  reg         queue_prk_ready_6;
  reg         queue_prk_ready_7;
  reg  [3:0]  tail;
  reg  [7:0]  mask;
  wire        is_pop = mask[0] & queue_prj_ready_0 & queue_prk_ready_0 & ~io_stall;
  wire [3:0]  _tail_pop_T = 4'(tail - {3'h0, is_pop});
  wire [7:0]  _mask_pop_T_1 = {1'h0, mask[7:1]};
  wire [7:0]  mask_pop = is_pop ? _mask_pop_T_1 : mask;
  wire [7:0]  mask_keep = is_pop ? 8'h0 : mask;
  wire [7:0]  _mask_shift_T = ~mask_keep;
  wire        to_insert_inst_inst_valid_0 =
    io_insts_valid_0 ? io_insts_inst_valid_0 : io_insts_inst_valid_1;
  wire        to_insert_inst_rd_valid_0 =
    io_insts_valid_0 ? io_insts_rd_valid_0 : io_insts_rd_valid_1;
  wire [31:0] to_insert_inst_imm_0 = io_insts_valid_0 ? io_insts_imm_0 : io_insts_imm_1;
  wire [3:0]  to_insert_inst_alu_op_0 =
    io_insts_valid_0 ? io_insts_alu_op_0 : io_insts_alu_op_1;
  wire [4:0]  to_insert_inst_mem_type_0 =
    io_insts_valid_0 ? io_insts_mem_type_0 : io_insts_mem_type_1;
  wire [12:0] to_insert_inst_priv_vec_0 =
    io_insts_valid_0 ? io_insts_priv_vec_0 : io_insts_priv_vec_1;
  wire [5:0]  to_insert_inst_prj_0 = io_insts_valid_0 ? io_insts_prj_0 : io_insts_prj_1;
  wire [5:0]  to_insert_inst_prk_0 = io_insts_valid_0 ? io_insts_prk_0 : io_insts_prk_1;
  wire [5:0]  to_insert_inst_prd_0 = io_insts_valid_0 ? io_insts_prd_0 : io_insts_prd_1;
  wire [4:0]  to_insert_inst_rob_index_0 =
    io_insts_valid_0 ? io_insts_rob_index_0 : io_insts_rob_index_1;
  wire        to_insert_prj_ready_0 = io_insts_valid_0 ? io_prj_ready_0 : io_prj_ready_1;
  wire        to_insert_prk_ready_0 = io_insts_valid_0 ? io_prk_ready_0 : io_prk_ready_1;
  wire [1:0]  _mask_T_4 = {io_insts_valid_1, io_insts_valid_0};
  wire        _GEN = mask_pop[0] & _mask_shift_T[0];
  wire        _GEN_0 = (|_mask_T_4) & _tail_pop_T == 4'h0;
  wire [3:0]  _GEN_1 = 4'(_tail_pop_T + 4'h1);
  wire        _GEN_2 = (&_mask_T_4) & _GEN_1 == 4'h0;
  wire [5:0]  _GEN_3 = _GEN_0 ? to_insert_inst_prj_0 : 6'h0;
  wire [5:0]  _GEN_4 = _GEN_2 ? io_insts_prj_1 : _GEN_3;
  wire [5:0]  _GEN_5 = _GEN ? queue_inst_prj_1 : _GEN_4;
  wire [5:0]  queue_nxt_inst_prj = mask_keep[0] ? queue_inst_prj_0 : _GEN_5;
  wire [5:0]  _GEN_6 = _GEN_0 ? to_insert_inst_prk_0 : 6'h0;
  wire [5:0]  _GEN_7 = _GEN_2 ? io_insts_prk_1 : _GEN_6;
  wire [5:0]  _GEN_8 = _GEN ? queue_inst_prk_1 : _GEN_7;
  wire [5:0]  queue_nxt_inst_prk = mask_keep[0] ? queue_inst_prk_0 : _GEN_8;
  wire        _GEN_9 = mask_pop[1] & _mask_shift_T[1];
  wire        _GEN_10 = (|_mask_T_4) & _tail_pop_T == 4'h1;
  wire        _GEN_11 = (&_mask_T_4) & _GEN_1 == 4'h1;
  wire [5:0]  _GEN_12 = _GEN_10 ? to_insert_inst_prj_0 : 6'h0;
  wire [5:0]  _GEN_13 = _GEN_11 ? io_insts_prj_1 : _GEN_12;
  wire [5:0]  _GEN_14 = _GEN_9 ? queue_inst_prj_2 : _GEN_13;
  wire [5:0]  queue_nxt_1_inst_prj = mask_keep[1] ? queue_inst_prj_1 : _GEN_14;
  wire [5:0]  _GEN_15 = _GEN_10 ? to_insert_inst_prk_0 : 6'h0;
  wire [5:0]  _GEN_16 = _GEN_11 ? io_insts_prk_1 : _GEN_15;
  wire [5:0]  _GEN_17 = _GEN_9 ? queue_inst_prk_2 : _GEN_16;
  wire [5:0]  queue_nxt_1_inst_prk = mask_keep[1] ? queue_inst_prk_1 : _GEN_17;
  wire        _GEN_18 = mask_pop[2] & _mask_shift_T[2];
  wire        _GEN_19 = (|_mask_T_4) & _tail_pop_T == 4'h2;
  wire        _GEN_20 = (&_mask_T_4) & _GEN_1 == 4'h2;
  wire [5:0]  _GEN_21 = _GEN_19 ? to_insert_inst_prj_0 : 6'h0;
  wire [5:0]  _GEN_22 = _GEN_20 ? io_insts_prj_1 : _GEN_21;
  wire [5:0]  _GEN_23 = _GEN_18 ? queue_inst_prj_3 : _GEN_22;
  wire [5:0]  queue_nxt_2_inst_prj = mask_keep[2] ? queue_inst_prj_2 : _GEN_23;
  wire [5:0]  _GEN_24 = _GEN_19 ? to_insert_inst_prk_0 : 6'h0;
  wire [5:0]  _GEN_25 = _GEN_20 ? io_insts_prk_1 : _GEN_24;
  wire [5:0]  _GEN_26 = _GEN_18 ? queue_inst_prk_3 : _GEN_25;
  wire [5:0]  queue_nxt_2_inst_prk = mask_keep[2] ? queue_inst_prk_2 : _GEN_26;
  wire        _GEN_27 = mask_pop[3] & _mask_shift_T[3];
  wire        _GEN_28 = (|_mask_T_4) & _tail_pop_T == 4'h3;
  wire        _GEN_29 = (&_mask_T_4) & _GEN_1 == 4'h3;
  wire [5:0]  _GEN_30 = _GEN_28 ? to_insert_inst_prj_0 : 6'h0;
  wire [5:0]  _GEN_31 = _GEN_29 ? io_insts_prj_1 : _GEN_30;
  wire [5:0]  _GEN_32 = _GEN_27 ? queue_inst_prj_4 : _GEN_31;
  wire [5:0]  queue_nxt_3_inst_prj = mask_keep[3] ? queue_inst_prj_3 : _GEN_32;
  wire [5:0]  _GEN_33 = _GEN_28 ? to_insert_inst_prk_0 : 6'h0;
  wire [5:0]  _GEN_34 = _GEN_29 ? io_insts_prk_1 : _GEN_33;
  wire [5:0]  _GEN_35 = _GEN_27 ? queue_inst_prk_4 : _GEN_34;
  wire [5:0]  queue_nxt_3_inst_prk = mask_keep[3] ? queue_inst_prk_3 : _GEN_35;
  wire        _GEN_36 = mask_pop[4] & _mask_shift_T[4];
  wire        _GEN_37 = (|_mask_T_4) & _tail_pop_T == 4'h4;
  wire        _GEN_38 = (&_mask_T_4) & _GEN_1 == 4'h4;
  wire [5:0]  _GEN_39 = _GEN_37 ? to_insert_inst_prj_0 : 6'h0;
  wire [5:0]  _GEN_40 = _GEN_38 ? io_insts_prj_1 : _GEN_39;
  wire [5:0]  _GEN_41 = _GEN_36 ? queue_inst_prj_5 : _GEN_40;
  wire [5:0]  queue_nxt_4_inst_prj = mask_keep[4] ? queue_inst_prj_4 : _GEN_41;
  wire [5:0]  _GEN_42 = _GEN_37 ? to_insert_inst_prk_0 : 6'h0;
  wire [5:0]  _GEN_43 = _GEN_38 ? io_insts_prk_1 : _GEN_42;
  wire [5:0]  _GEN_44 = _GEN_36 ? queue_inst_prk_5 : _GEN_43;
  wire [5:0]  queue_nxt_4_inst_prk = mask_keep[4] ? queue_inst_prk_4 : _GEN_44;
  wire        _GEN_45 = mask_pop[5] & _mask_shift_T[5];
  wire        _GEN_46 = (|_mask_T_4) & _tail_pop_T == 4'h5;
  wire        _GEN_47 = (&_mask_T_4) & _GEN_1 == 4'h5;
  wire [5:0]  _GEN_48 = _GEN_46 ? to_insert_inst_prj_0 : 6'h0;
  wire [5:0]  _GEN_49 = _GEN_47 ? io_insts_prj_1 : _GEN_48;
  wire [5:0]  _GEN_50 = _GEN_45 ? queue_inst_prj_6 : _GEN_49;
  wire [5:0]  queue_nxt_5_inst_prj = mask_keep[5] ? queue_inst_prj_5 : _GEN_50;
  wire [5:0]  _GEN_51 = _GEN_46 ? to_insert_inst_prk_0 : 6'h0;
  wire [5:0]  _GEN_52 = _GEN_47 ? io_insts_prk_1 : _GEN_51;
  wire [5:0]  _GEN_53 = _GEN_45 ? queue_inst_prk_6 : _GEN_52;
  wire [5:0]  queue_nxt_5_inst_prk = mask_keep[5] ? queue_inst_prk_5 : _GEN_53;
  wire        _GEN_54 = mask_pop[6] & _mask_shift_T[6];
  wire        _GEN_55 = (|_mask_T_4) & _tail_pop_T == 4'h6;
  wire        _GEN_56 = (&_mask_T_4) & _GEN_1 == 4'h6;
  wire [5:0]  _GEN_57 = _GEN_55 ? to_insert_inst_prj_0 : 6'h0;
  wire [5:0]  _GEN_58 = _GEN_56 ? io_insts_prj_1 : _GEN_57;
  wire [5:0]  _GEN_59 = _GEN_54 ? queue_inst_prj_7 : _GEN_58;
  wire [5:0]  queue_nxt_6_inst_prj = mask_keep[6] ? queue_inst_prj_6 : _GEN_59;
  wire [5:0]  _GEN_60 = _GEN_55 ? to_insert_inst_prk_0 : 6'h0;
  wire [5:0]  _GEN_61 = _GEN_56 ? io_insts_prk_1 : _GEN_60;
  wire [5:0]  _GEN_62 = _GEN_54 ? queue_inst_prk_7 : _GEN_61;
  wire [5:0]  queue_nxt_6_inst_prk = mask_keep[6] ? queue_inst_prk_6 : _GEN_62;
  wire        _GEN_63 = mask_pop[7] & _mask_shift_T[7];
  wire        _GEN_64 = (|_mask_T_4) & _tail_pop_T == 4'h7;
  wire        _GEN_65 = (&_mask_T_4) & _GEN_1 == 4'h7;
  wire [5:0]  _GEN_66 = _GEN_64 ? to_insert_inst_prj_0 : 6'h0;
  wire [5:0]  _GEN_67 = _GEN_65 ? io_insts_prj_1 : _GEN_66;
  wire [5:0]  _GEN_68 = _GEN_63 ? 6'h0 : _GEN_67;
  wire [5:0]  queue_nxt_7_inst_prj = mask_keep[7] ? queue_inst_prj_7 : _GEN_68;
  wire [5:0]  _GEN_69 = _GEN_64 ? to_insert_inst_prk_0 : 6'h0;
  wire [5:0]  _GEN_70 = _GEN_65 ? io_insts_prk_1 : _GEN_69;
  wire [5:0]  _GEN_71 = _GEN_63 ? 6'h0 : _GEN_70;
  wire [5:0]  queue_nxt_7_inst_prk = mask_keep[7] ? queue_inst_prk_7 : _GEN_71;
  wire        _GEN_72 =
    _GEN_2 ? io_insts_inst_valid_1 : _GEN_0 & to_insert_inst_inst_valid_0;
  wire        _GEN_73 = _GEN_2 ? io_insts_rd_valid_1 : _GEN_0 & to_insert_inst_rd_valid_0;
  wire [31:0] _GEN_74 = _GEN_0 ? to_insert_inst_imm_0 : 32'h0;
  wire [31:0] _GEN_75 = _GEN_2 ? io_insts_imm_1 : _GEN_74;
  wire [3:0]  _GEN_76 = _GEN_0 ? to_insert_inst_alu_op_0 : 4'h0;
  wire [3:0]  _GEN_77 = _GEN_2 ? io_insts_alu_op_1 : _GEN_76;
  wire [4:0]  _GEN_78 = _GEN_0 ? to_insert_inst_mem_type_0 : 5'h0;
  wire [4:0]  _GEN_79 = _GEN_2 ? io_insts_mem_type_1 : _GEN_78;
  wire [12:0] _GEN_80 = _GEN_0 ? to_insert_inst_priv_vec_0 : 13'h0;
  wire [12:0] _GEN_81 = _GEN_2 ? io_insts_priv_vec_1 : _GEN_80;
  wire [5:0]  _GEN_82 = _GEN_0 ? to_insert_inst_prd_0 : 6'h0;
  wire [5:0]  _GEN_83 = _GEN_2 ? io_insts_prd_1 : _GEN_82;
  wire [4:0]  _GEN_84 = _GEN_0 ? to_insert_inst_rob_index_0 : 5'h0;
  wire [4:0]  _GEN_85 = _GEN_2 ? io_insts_rob_index_1 : _GEN_84;
  wire        _GEN_86 =
    _GEN_11 ? io_insts_inst_valid_1 : _GEN_10 & to_insert_inst_inst_valid_0;
  wire        _GEN_87 =
    _GEN_11 ? io_insts_rd_valid_1 : _GEN_10 & to_insert_inst_rd_valid_0;
  wire [31:0] _GEN_88 = _GEN_10 ? to_insert_inst_imm_0 : 32'h0;
  wire [31:0] _GEN_89 = _GEN_11 ? io_insts_imm_1 : _GEN_88;
  wire [3:0]  _GEN_90 = _GEN_10 ? to_insert_inst_alu_op_0 : 4'h0;
  wire [3:0]  _GEN_91 = _GEN_11 ? io_insts_alu_op_1 : _GEN_90;
  wire [4:0]  _GEN_92 = _GEN_10 ? to_insert_inst_mem_type_0 : 5'h0;
  wire [4:0]  _GEN_93 = _GEN_11 ? io_insts_mem_type_1 : _GEN_92;
  wire [12:0] _GEN_94 = _GEN_10 ? to_insert_inst_priv_vec_0 : 13'h0;
  wire [12:0] _GEN_95 = _GEN_11 ? io_insts_priv_vec_1 : _GEN_94;
  wire [5:0]  _GEN_96 = _GEN_10 ? to_insert_inst_prd_0 : 6'h0;
  wire [5:0]  _GEN_97 = _GEN_11 ? io_insts_prd_1 : _GEN_96;
  wire [4:0]  _GEN_98 = _GEN_10 ? to_insert_inst_rob_index_0 : 5'h0;
  wire [4:0]  _GEN_99 = _GEN_11 ? io_insts_rob_index_1 : _GEN_98;
  wire        _GEN_100 =
    _GEN_20 ? io_insts_inst_valid_1 : _GEN_19 & to_insert_inst_inst_valid_0;
  wire        _GEN_101 =
    _GEN_20 ? io_insts_rd_valid_1 : _GEN_19 & to_insert_inst_rd_valid_0;
  wire [31:0] _GEN_102 = _GEN_19 ? to_insert_inst_imm_0 : 32'h0;
  wire [31:0] _GEN_103 = _GEN_20 ? io_insts_imm_1 : _GEN_102;
  wire [3:0]  _GEN_104 = _GEN_19 ? to_insert_inst_alu_op_0 : 4'h0;
  wire [3:0]  _GEN_105 = _GEN_20 ? io_insts_alu_op_1 : _GEN_104;
  wire [4:0]  _GEN_106 = _GEN_19 ? to_insert_inst_mem_type_0 : 5'h0;
  wire [4:0]  _GEN_107 = _GEN_20 ? io_insts_mem_type_1 : _GEN_106;
  wire [12:0] _GEN_108 = _GEN_19 ? to_insert_inst_priv_vec_0 : 13'h0;
  wire [12:0] _GEN_109 = _GEN_20 ? io_insts_priv_vec_1 : _GEN_108;
  wire [5:0]  _GEN_110 = _GEN_19 ? to_insert_inst_prd_0 : 6'h0;
  wire [5:0]  _GEN_111 = _GEN_20 ? io_insts_prd_1 : _GEN_110;
  wire [4:0]  _GEN_112 = _GEN_19 ? to_insert_inst_rob_index_0 : 5'h0;
  wire [4:0]  _GEN_113 = _GEN_20 ? io_insts_rob_index_1 : _GEN_112;
  wire        _GEN_114 =
    _GEN_29 ? io_insts_inst_valid_1 : _GEN_28 & to_insert_inst_inst_valid_0;
  wire        _GEN_115 =
    _GEN_29 ? io_insts_rd_valid_1 : _GEN_28 & to_insert_inst_rd_valid_0;
  wire [31:0] _GEN_116 = _GEN_28 ? to_insert_inst_imm_0 : 32'h0;
  wire [31:0] _GEN_117 = _GEN_29 ? io_insts_imm_1 : _GEN_116;
  wire [3:0]  _GEN_118 = _GEN_28 ? to_insert_inst_alu_op_0 : 4'h0;
  wire [3:0]  _GEN_119 = _GEN_29 ? io_insts_alu_op_1 : _GEN_118;
  wire [4:0]  _GEN_120 = _GEN_28 ? to_insert_inst_mem_type_0 : 5'h0;
  wire [4:0]  _GEN_121 = _GEN_29 ? io_insts_mem_type_1 : _GEN_120;
  wire [12:0] _GEN_122 = _GEN_28 ? to_insert_inst_priv_vec_0 : 13'h0;
  wire [12:0] _GEN_123 = _GEN_29 ? io_insts_priv_vec_1 : _GEN_122;
  wire [5:0]  _GEN_124 = _GEN_28 ? to_insert_inst_prd_0 : 6'h0;
  wire [5:0]  _GEN_125 = _GEN_29 ? io_insts_prd_1 : _GEN_124;
  wire [4:0]  _GEN_126 = _GEN_28 ? to_insert_inst_rob_index_0 : 5'h0;
  wire [4:0]  _GEN_127 = _GEN_29 ? io_insts_rob_index_1 : _GEN_126;
  wire        _GEN_128 =
    _GEN_38 ? io_insts_inst_valid_1 : _GEN_37 & to_insert_inst_inst_valid_0;
  wire        _GEN_129 =
    _GEN_38 ? io_insts_rd_valid_1 : _GEN_37 & to_insert_inst_rd_valid_0;
  wire [31:0] _GEN_130 = _GEN_37 ? to_insert_inst_imm_0 : 32'h0;
  wire [31:0] _GEN_131 = _GEN_38 ? io_insts_imm_1 : _GEN_130;
  wire [3:0]  _GEN_132 = _GEN_37 ? to_insert_inst_alu_op_0 : 4'h0;
  wire [3:0]  _GEN_133 = _GEN_38 ? io_insts_alu_op_1 : _GEN_132;
  wire [4:0]  _GEN_134 = _GEN_37 ? to_insert_inst_mem_type_0 : 5'h0;
  wire [4:0]  _GEN_135 = _GEN_38 ? io_insts_mem_type_1 : _GEN_134;
  wire [12:0] _GEN_136 = _GEN_37 ? to_insert_inst_priv_vec_0 : 13'h0;
  wire [12:0] _GEN_137 = _GEN_38 ? io_insts_priv_vec_1 : _GEN_136;
  wire [5:0]  _GEN_138 = _GEN_37 ? to_insert_inst_prd_0 : 6'h0;
  wire [5:0]  _GEN_139 = _GEN_38 ? io_insts_prd_1 : _GEN_138;
  wire [4:0]  _GEN_140 = _GEN_37 ? to_insert_inst_rob_index_0 : 5'h0;
  wire [4:0]  _GEN_141 = _GEN_38 ? io_insts_rob_index_1 : _GEN_140;
  wire        _GEN_142 =
    _GEN_47 ? io_insts_inst_valid_1 : _GEN_46 & to_insert_inst_inst_valid_0;
  wire        _GEN_143 =
    _GEN_47 ? io_insts_rd_valid_1 : _GEN_46 & to_insert_inst_rd_valid_0;
  wire [31:0] _GEN_144 = _GEN_46 ? to_insert_inst_imm_0 : 32'h0;
  wire [31:0] _GEN_145 = _GEN_47 ? io_insts_imm_1 : _GEN_144;
  wire [3:0]  _GEN_146 = _GEN_46 ? to_insert_inst_alu_op_0 : 4'h0;
  wire [3:0]  _GEN_147 = _GEN_47 ? io_insts_alu_op_1 : _GEN_146;
  wire [4:0]  _GEN_148 = _GEN_46 ? to_insert_inst_mem_type_0 : 5'h0;
  wire [4:0]  _GEN_149 = _GEN_47 ? io_insts_mem_type_1 : _GEN_148;
  wire [12:0] _GEN_150 = _GEN_46 ? to_insert_inst_priv_vec_0 : 13'h0;
  wire [12:0] _GEN_151 = _GEN_47 ? io_insts_priv_vec_1 : _GEN_150;
  wire [5:0]  _GEN_152 = _GEN_46 ? to_insert_inst_prd_0 : 6'h0;
  wire [5:0]  _GEN_153 = _GEN_47 ? io_insts_prd_1 : _GEN_152;
  wire [4:0]  _GEN_154 = _GEN_46 ? to_insert_inst_rob_index_0 : 5'h0;
  wire [4:0]  _GEN_155 = _GEN_47 ? io_insts_rob_index_1 : _GEN_154;
  wire        _GEN_156 =
    _GEN_56 ? io_insts_inst_valid_1 : _GEN_55 & to_insert_inst_inst_valid_0;
  wire        _GEN_157 =
    _GEN_56 ? io_insts_rd_valid_1 : _GEN_55 & to_insert_inst_rd_valid_0;
  wire [31:0] _GEN_158 = _GEN_55 ? to_insert_inst_imm_0 : 32'h0;
  wire [31:0] _GEN_159 = _GEN_56 ? io_insts_imm_1 : _GEN_158;
  wire [3:0]  _GEN_160 = _GEN_55 ? to_insert_inst_alu_op_0 : 4'h0;
  wire [3:0]  _GEN_161 = _GEN_56 ? io_insts_alu_op_1 : _GEN_160;
  wire [4:0]  _GEN_162 = _GEN_55 ? to_insert_inst_mem_type_0 : 5'h0;
  wire [4:0]  _GEN_163 = _GEN_56 ? io_insts_mem_type_1 : _GEN_162;
  wire [12:0] _GEN_164 = _GEN_55 ? to_insert_inst_priv_vec_0 : 13'h0;
  wire [12:0] _GEN_165 = _GEN_56 ? io_insts_priv_vec_1 : _GEN_164;
  wire [5:0]  _GEN_166 = _GEN_55 ? to_insert_inst_prd_0 : 6'h0;
  wire [5:0]  _GEN_167 = _GEN_56 ? io_insts_prd_1 : _GEN_166;
  wire [4:0]  _GEN_168 = _GEN_55 ? to_insert_inst_rob_index_0 : 5'h0;
  wire [4:0]  _GEN_169 = _GEN_56 ? io_insts_rob_index_1 : _GEN_168;
  wire        _GEN_170 =
    _GEN_65 ? io_insts_inst_valid_1 : _GEN_64 & to_insert_inst_inst_valid_0;
  wire        _GEN_171 =
    _GEN_65 ? io_insts_rd_valid_1 : _GEN_64 & to_insert_inst_rd_valid_0;
  wire [31:0] _GEN_172 = _GEN_64 ? to_insert_inst_imm_0 : 32'h0;
  wire [31:0] _GEN_173 = _GEN_65 ? io_insts_imm_1 : _GEN_172;
  wire [3:0]  _GEN_174 = _GEN_64 ? to_insert_inst_alu_op_0 : 4'h0;
  wire [3:0]  _GEN_175 = _GEN_65 ? io_insts_alu_op_1 : _GEN_174;
  wire [4:0]  _GEN_176 = _GEN_64 ? to_insert_inst_mem_type_0 : 5'h0;
  wire [4:0]  _GEN_177 = _GEN_65 ? io_insts_mem_type_1 : _GEN_176;
  wire [12:0] _GEN_178 = _GEN_64 ? to_insert_inst_priv_vec_0 : 13'h0;
  wire [12:0] _GEN_179 = _GEN_65 ? io_insts_priv_vec_1 : _GEN_178;
  wire [5:0]  _GEN_180 = _GEN_64 ? to_insert_inst_prd_0 : 6'h0;
  wire [5:0]  _GEN_181 = _GEN_65 ? io_insts_prd_1 : _GEN_180;
  wire [4:0]  _GEN_182 = _GEN_64 ? to_insert_inst_rob_index_0 : 5'h0;
  wire [4:0]  _GEN_183 = _GEN_65 ? io_insts_rob_index_1 : _GEN_182;
  wire        _GEN_184 = _GEN_2 ? io_prj_ready_1 : _GEN_0 & to_insert_prj_ready_0;
  wire        _GEN_185 = _GEN ? queue_prj_ready_1 : _GEN_184;
  wire        _GEN_186 = mask_keep[0] ? queue_prj_ready_0 : _GEN_185;
  wire        _GEN_187 = _GEN_11 ? io_prj_ready_1 : _GEN_10 & to_insert_prj_ready_0;
  wire        _GEN_188 = _GEN_9 ? queue_prj_ready_2 : _GEN_187;
  wire        _GEN_189 = mask_keep[1] ? queue_prj_ready_1 : _GEN_188;
  wire        _GEN_190 = _GEN_20 ? io_prj_ready_1 : _GEN_19 & to_insert_prj_ready_0;
  wire        _GEN_191 = _GEN_18 ? queue_prj_ready_3 : _GEN_190;
  wire        _GEN_192 = mask_keep[2] ? queue_prj_ready_2 : _GEN_191;
  wire        _GEN_193 = _GEN_29 ? io_prj_ready_1 : _GEN_28 & to_insert_prj_ready_0;
  wire        _GEN_194 = _GEN_27 ? queue_prj_ready_4 : _GEN_193;
  wire        _GEN_195 = mask_keep[3] ? queue_prj_ready_3 : _GEN_194;
  wire        _GEN_196 = _GEN_38 ? io_prj_ready_1 : _GEN_37 & to_insert_prj_ready_0;
  wire        _GEN_197 = _GEN_36 ? queue_prj_ready_5 : _GEN_196;
  wire        _GEN_198 = mask_keep[4] ? queue_prj_ready_4 : _GEN_197;
  wire        _GEN_199 = _GEN_47 ? io_prj_ready_1 : _GEN_46 & to_insert_prj_ready_0;
  wire        _GEN_200 = _GEN_45 ? queue_prj_ready_6 : _GEN_199;
  wire        _GEN_201 = mask_keep[5] ? queue_prj_ready_5 : _GEN_200;
  wire        _GEN_202 = _GEN_56 ? io_prj_ready_1 : _GEN_55 & to_insert_prj_ready_0;
  wire        _GEN_203 = _GEN_54 ? queue_prj_ready_7 : _GEN_202;
  wire        _GEN_204 = mask_keep[6] ? queue_prj_ready_6 : _GEN_203;
  wire        _GEN_205 = _GEN_65 ? io_prj_ready_1 : _GEN_64 & to_insert_prj_ready_0;
  wire        _GEN_206 = mask_keep[7] ? queue_prj_ready_7 : ~_GEN_63 & _GEN_205;
  wire        _GEN_207 = _GEN_2 ? io_prk_ready_1 : _GEN_0 & to_insert_prk_ready_0;
  wire        _GEN_208 = _GEN ? queue_prk_ready_1 : _GEN_207;
  wire        _GEN_209 = mask_keep[0] ? queue_prk_ready_0 : _GEN_208;
  wire        _GEN_210 = _GEN_11 ? io_prk_ready_1 : _GEN_10 & to_insert_prk_ready_0;
  wire        _GEN_211 = _GEN_9 ? queue_prk_ready_2 : _GEN_210;
  wire        _GEN_212 = mask_keep[1] ? queue_prk_ready_1 : _GEN_211;
  wire        _GEN_213 = _GEN_20 ? io_prk_ready_1 : _GEN_19 & to_insert_prk_ready_0;
  wire        _GEN_214 = _GEN_18 ? queue_prk_ready_3 : _GEN_213;
  wire        _GEN_215 = mask_keep[2] ? queue_prk_ready_2 : _GEN_214;
  wire        _GEN_216 = _GEN_29 ? io_prk_ready_1 : _GEN_28 & to_insert_prk_ready_0;
  wire        _GEN_217 = _GEN_27 ? queue_prk_ready_4 : _GEN_216;
  wire        _GEN_218 = mask_keep[3] ? queue_prk_ready_3 : _GEN_217;
  wire        _GEN_219 = _GEN_38 ? io_prk_ready_1 : _GEN_37 & to_insert_prk_ready_0;
  wire        _GEN_220 = _GEN_36 ? queue_prk_ready_5 : _GEN_219;
  wire        _GEN_221 = mask_keep[4] ? queue_prk_ready_4 : _GEN_220;
  wire        _GEN_222 = _GEN_47 ? io_prk_ready_1 : _GEN_46 & to_insert_prk_ready_0;
  wire        _GEN_223 = _GEN_45 ? queue_prk_ready_6 : _GEN_222;
  wire        _GEN_224 = mask_keep[5] ? queue_prk_ready_5 : _GEN_223;
  wire        _GEN_225 = _GEN_56 ? io_prk_ready_1 : _GEN_55 & to_insert_prk_ready_0;
  wire        _GEN_226 = _GEN_54 ? queue_prk_ready_7 : _GEN_225;
  wire        _GEN_227 = mask_keep[6] ? queue_prk_ready_6 : _GEN_226;
  wire        _GEN_228 = _GEN_65 ? io_prk_ready_1 : _GEN_64 & to_insert_prk_ready_0;
  wire        _GEN_229 = mask_keep[7] ? queue_prk_ready_7 : ~_GEN_63 & _GEN_228;
  always @(posedge clock) begin
    if (reset) begin
      queue_inst_inst_valid_0 <= 1'h0;
      queue_inst_inst_valid_1 <= 1'h0;
      queue_inst_inst_valid_2 <= 1'h0;
      queue_inst_inst_valid_3 <= 1'h0;
      queue_inst_inst_valid_4 <= 1'h0;
      queue_inst_inst_valid_5 <= 1'h0;
      queue_inst_inst_valid_6 <= 1'h0;
      queue_inst_inst_valid_7 <= 1'h0;
      queue_inst_rd_valid_0 <= 1'h0;
      queue_inst_rd_valid_1 <= 1'h0;
      queue_inst_rd_valid_2 <= 1'h0;
      queue_inst_rd_valid_3 <= 1'h0;
      queue_inst_rd_valid_4 <= 1'h0;
      queue_inst_rd_valid_5 <= 1'h0;
      queue_inst_rd_valid_6 <= 1'h0;
      queue_inst_rd_valid_7 <= 1'h0;
      queue_inst_imm_0 <= 32'h0;
      queue_inst_imm_1 <= 32'h0;
      queue_inst_imm_2 <= 32'h0;
      queue_inst_imm_3 <= 32'h0;
      queue_inst_imm_4 <= 32'h0;
      queue_inst_imm_5 <= 32'h0;
      queue_inst_imm_6 <= 32'h0;
      queue_inst_imm_7 <= 32'h0;
      queue_inst_alu_op_0 <= 4'h0;
      queue_inst_alu_op_1 <= 4'h0;
      queue_inst_alu_op_2 <= 4'h0;
      queue_inst_alu_op_3 <= 4'h0;
      queue_inst_alu_op_4 <= 4'h0;
      queue_inst_alu_op_5 <= 4'h0;
      queue_inst_alu_op_6 <= 4'h0;
      queue_inst_alu_op_7 <= 4'h0;
      queue_inst_mem_type_0 <= 5'h0;
      queue_inst_mem_type_1 <= 5'h0;
      queue_inst_mem_type_2 <= 5'h0;
      queue_inst_mem_type_3 <= 5'h0;
      queue_inst_mem_type_4 <= 5'h0;
      queue_inst_mem_type_5 <= 5'h0;
      queue_inst_mem_type_6 <= 5'h0;
      queue_inst_mem_type_7 <= 5'h0;
      queue_inst_priv_vec_0 <= 13'h0;
      queue_inst_priv_vec_1 <= 13'h0;
      queue_inst_priv_vec_2 <= 13'h0;
      queue_inst_priv_vec_3 <= 13'h0;
      queue_inst_priv_vec_4 <= 13'h0;
      queue_inst_priv_vec_5 <= 13'h0;
      queue_inst_priv_vec_6 <= 13'h0;
      queue_inst_priv_vec_7 <= 13'h0;
      queue_inst_prj_0 <= 6'h0;
      queue_inst_prj_1 <= 6'h0;
      queue_inst_prj_2 <= 6'h0;
      queue_inst_prj_3 <= 6'h0;
      queue_inst_prj_4 <= 6'h0;
      queue_inst_prj_5 <= 6'h0;
      queue_inst_prj_6 <= 6'h0;
      queue_inst_prj_7 <= 6'h0;
      queue_inst_prk_0 <= 6'h0;
      queue_inst_prk_1 <= 6'h0;
      queue_inst_prk_2 <= 6'h0;
      queue_inst_prk_3 <= 6'h0;
      queue_inst_prk_4 <= 6'h0;
      queue_inst_prk_5 <= 6'h0;
      queue_inst_prk_6 <= 6'h0;
      queue_inst_prk_7 <= 6'h0;
      queue_inst_prd_0 <= 6'h0;
      queue_inst_prd_1 <= 6'h0;
      queue_inst_prd_2 <= 6'h0;
      queue_inst_prd_3 <= 6'h0;
      queue_inst_prd_4 <= 6'h0;
      queue_inst_prd_5 <= 6'h0;
      queue_inst_prd_6 <= 6'h0;
      queue_inst_prd_7 <= 6'h0;
      queue_inst_rob_index_0 <= 5'h0;
      queue_inst_rob_index_1 <= 5'h0;
      queue_inst_rob_index_2 <= 5'h0;
      queue_inst_rob_index_3 <= 5'h0;
      queue_inst_rob_index_4 <= 5'h0;
      queue_inst_rob_index_5 <= 5'h0;
      queue_inst_rob_index_6 <= 5'h0;
      queue_inst_rob_index_7 <= 5'h0;
      queue_prj_ready_0 <= 1'h0;
      queue_prj_ready_1 <= 1'h0;
      queue_prj_ready_2 <= 1'h0;
      queue_prj_ready_3 <= 1'h0;
      queue_prj_ready_4 <= 1'h0;
      queue_prj_ready_5 <= 1'h0;
      queue_prj_ready_6 <= 1'h0;
      queue_prj_ready_7 <= 1'h0;
      queue_prk_ready_0 <= 1'h0;
      queue_prk_ready_1 <= 1'h0;
      queue_prk_ready_2 <= 1'h0;
      queue_prk_ready_3 <= 1'h0;
      queue_prk_ready_4 <= 1'h0;
      queue_prk_ready_5 <= 1'h0;
      queue_prk_ready_6 <= 1'h0;
      queue_prk_ready_7 <= 1'h0;
      tail <= 4'h0;
      mask <= 8'h0;
    end
    else begin
      if (mask_keep[0]) begin
      end
      else begin
        queue_inst_inst_valid_0 <= _GEN ? queue_inst_inst_valid_1 : _GEN_72;
        queue_inst_rd_valid_0 <= _GEN ? queue_inst_rd_valid_1 : _GEN_73;
        queue_inst_imm_0 <= _GEN ? queue_inst_imm_1 : _GEN_75;
        queue_inst_alu_op_0 <= _GEN ? queue_inst_alu_op_1 : _GEN_77;
        queue_inst_mem_type_0 <= _GEN ? queue_inst_mem_type_1 : _GEN_79;
        queue_inst_priv_vec_0 <= _GEN ? queue_inst_priv_vec_1 : _GEN_81;
        queue_inst_prj_0 <= _GEN_5;
        queue_inst_prk_0 <= _GEN_8;
        queue_inst_prd_0 <= _GEN ? queue_inst_prd_1 : _GEN_83;
        queue_inst_rob_index_0 <= _GEN ? queue_inst_rob_index_1 : _GEN_85;
      end
      if (mask_keep[1]) begin
      end
      else begin
        queue_inst_inst_valid_1 <= _GEN_9 ? queue_inst_inst_valid_2 : _GEN_86;
        queue_inst_rd_valid_1 <= _GEN_9 ? queue_inst_rd_valid_2 : _GEN_87;
        queue_inst_imm_1 <= _GEN_9 ? queue_inst_imm_2 : _GEN_89;
        queue_inst_alu_op_1 <= _GEN_9 ? queue_inst_alu_op_2 : _GEN_91;
        queue_inst_mem_type_1 <= _GEN_9 ? queue_inst_mem_type_2 : _GEN_93;
        queue_inst_priv_vec_1 <= _GEN_9 ? queue_inst_priv_vec_2 : _GEN_95;
        queue_inst_prj_1 <= _GEN_14;
        queue_inst_prk_1 <= _GEN_17;
        queue_inst_prd_1 <= _GEN_9 ? queue_inst_prd_2 : _GEN_97;
        queue_inst_rob_index_1 <= _GEN_9 ? queue_inst_rob_index_2 : _GEN_99;
      end
      if (mask_keep[2]) begin
      end
      else begin
        queue_inst_inst_valid_2 <= _GEN_18 ? queue_inst_inst_valid_3 : _GEN_100;
        queue_inst_rd_valid_2 <= _GEN_18 ? queue_inst_rd_valid_3 : _GEN_101;
        queue_inst_imm_2 <= _GEN_18 ? queue_inst_imm_3 : _GEN_103;
        queue_inst_alu_op_2 <= _GEN_18 ? queue_inst_alu_op_3 : _GEN_105;
        queue_inst_mem_type_2 <= _GEN_18 ? queue_inst_mem_type_3 : _GEN_107;
        queue_inst_priv_vec_2 <= _GEN_18 ? queue_inst_priv_vec_3 : _GEN_109;
        queue_inst_prj_2 <= _GEN_23;
        queue_inst_prk_2 <= _GEN_26;
        queue_inst_prd_2 <= _GEN_18 ? queue_inst_prd_3 : _GEN_111;
        queue_inst_rob_index_2 <= _GEN_18 ? queue_inst_rob_index_3 : _GEN_113;
      end
      if (mask_keep[3]) begin
      end
      else begin
        queue_inst_inst_valid_3 <= _GEN_27 ? queue_inst_inst_valid_4 : _GEN_114;
        queue_inst_rd_valid_3 <= _GEN_27 ? queue_inst_rd_valid_4 : _GEN_115;
        queue_inst_imm_3 <= _GEN_27 ? queue_inst_imm_4 : _GEN_117;
        queue_inst_alu_op_3 <= _GEN_27 ? queue_inst_alu_op_4 : _GEN_119;
        queue_inst_mem_type_3 <= _GEN_27 ? queue_inst_mem_type_4 : _GEN_121;
        queue_inst_priv_vec_3 <= _GEN_27 ? queue_inst_priv_vec_4 : _GEN_123;
        queue_inst_prj_3 <= _GEN_32;
        queue_inst_prk_3 <= _GEN_35;
        queue_inst_prd_3 <= _GEN_27 ? queue_inst_prd_4 : _GEN_125;
        queue_inst_rob_index_3 <= _GEN_27 ? queue_inst_rob_index_4 : _GEN_127;
      end
      if (mask_keep[4]) begin
      end
      else begin
        queue_inst_inst_valid_4 <= _GEN_36 ? queue_inst_inst_valid_5 : _GEN_128;
        queue_inst_rd_valid_4 <= _GEN_36 ? queue_inst_rd_valid_5 : _GEN_129;
        queue_inst_imm_4 <= _GEN_36 ? queue_inst_imm_5 : _GEN_131;
        queue_inst_alu_op_4 <= _GEN_36 ? queue_inst_alu_op_5 : _GEN_133;
        queue_inst_mem_type_4 <= _GEN_36 ? queue_inst_mem_type_5 : _GEN_135;
        queue_inst_priv_vec_4 <= _GEN_36 ? queue_inst_priv_vec_5 : _GEN_137;
        queue_inst_prj_4 <= _GEN_41;
        queue_inst_prk_4 <= _GEN_44;
        queue_inst_prd_4 <= _GEN_36 ? queue_inst_prd_5 : _GEN_139;
        queue_inst_rob_index_4 <= _GEN_36 ? queue_inst_rob_index_5 : _GEN_141;
      end
      if (mask_keep[5]) begin
      end
      else begin
        queue_inst_inst_valid_5 <= _GEN_45 ? queue_inst_inst_valid_6 : _GEN_142;
        queue_inst_rd_valid_5 <= _GEN_45 ? queue_inst_rd_valid_6 : _GEN_143;
        queue_inst_imm_5 <= _GEN_45 ? queue_inst_imm_6 : _GEN_145;
        queue_inst_alu_op_5 <= _GEN_45 ? queue_inst_alu_op_6 : _GEN_147;
        queue_inst_mem_type_5 <= _GEN_45 ? queue_inst_mem_type_6 : _GEN_149;
        queue_inst_priv_vec_5 <= _GEN_45 ? queue_inst_priv_vec_6 : _GEN_151;
        queue_inst_prj_5 <= _GEN_50;
        queue_inst_prk_5 <= _GEN_53;
        queue_inst_prd_5 <= _GEN_45 ? queue_inst_prd_6 : _GEN_153;
        queue_inst_rob_index_5 <= _GEN_45 ? queue_inst_rob_index_6 : _GEN_155;
      end
      if (mask_keep[6]) begin
      end
      else begin
        queue_inst_inst_valid_6 <= _GEN_54 ? queue_inst_inst_valid_7 : _GEN_156;
        queue_inst_rd_valid_6 <= _GEN_54 ? queue_inst_rd_valid_7 : _GEN_157;
        queue_inst_imm_6 <= _GEN_54 ? queue_inst_imm_7 : _GEN_159;
        queue_inst_alu_op_6 <= _GEN_54 ? queue_inst_alu_op_7 : _GEN_161;
        queue_inst_mem_type_6 <= _GEN_54 ? queue_inst_mem_type_7 : _GEN_163;
        queue_inst_priv_vec_6 <= _GEN_54 ? queue_inst_priv_vec_7 : _GEN_165;
        queue_inst_prj_6 <= _GEN_59;
        queue_inst_prk_6 <= _GEN_62;
        queue_inst_prd_6 <= _GEN_54 ? queue_inst_prd_7 : _GEN_167;
        queue_inst_rob_index_6 <= _GEN_54 ? queue_inst_rob_index_7 : _GEN_169;
      end
      if (mask_keep[7]) begin
      end
      else begin
        queue_inst_inst_valid_7 <= ~_GEN_63 & _GEN_170;
        queue_inst_rd_valid_7 <= ~_GEN_63 & _GEN_171;
        queue_inst_imm_7 <= _GEN_63 ? 32'h0 : _GEN_173;
        queue_inst_alu_op_7 <= _GEN_63 ? 4'h0 : _GEN_175;
        queue_inst_mem_type_7 <= _GEN_63 ? 5'h0 : _GEN_177;
        queue_inst_priv_vec_7 <= _GEN_63 ? 13'h0 : _GEN_179;
        queue_inst_prj_7 <= _GEN_68;
        queue_inst_prk_7 <= _GEN_71;
        queue_inst_prd_7 <= _GEN_63 ? 6'h0 : _GEN_181;
        queue_inst_rob_index_7 <= _GEN_63 ? 5'h0 : _GEN_183;
      end
      queue_prj_ready_0 <=
        (|{io_wake_preg_3 == queue_nxt_inst_prj,
           io_wake_preg_2 == queue_nxt_inst_prj,
           io_wake_preg_1 == queue_nxt_inst_prj,
           io_wake_preg_0 == queue_nxt_inst_prj}) | _GEN_186;
      queue_prj_ready_1 <=
        (|{io_wake_preg_3 == queue_nxt_1_inst_prj,
           io_wake_preg_2 == queue_nxt_1_inst_prj,
           io_wake_preg_1 == queue_nxt_1_inst_prj,
           io_wake_preg_0 == queue_nxt_1_inst_prj}) | _GEN_189;
      queue_prj_ready_2 <=
        (|{io_wake_preg_3 == queue_nxt_2_inst_prj,
           io_wake_preg_2 == queue_nxt_2_inst_prj,
           io_wake_preg_1 == queue_nxt_2_inst_prj,
           io_wake_preg_0 == queue_nxt_2_inst_prj}) | _GEN_192;
      queue_prj_ready_3 <=
        (|{io_wake_preg_3 == queue_nxt_3_inst_prj,
           io_wake_preg_2 == queue_nxt_3_inst_prj,
           io_wake_preg_1 == queue_nxt_3_inst_prj,
           io_wake_preg_0 == queue_nxt_3_inst_prj}) | _GEN_195;
      queue_prj_ready_4 <=
        (|{io_wake_preg_3 == queue_nxt_4_inst_prj,
           io_wake_preg_2 == queue_nxt_4_inst_prj,
           io_wake_preg_1 == queue_nxt_4_inst_prj,
           io_wake_preg_0 == queue_nxt_4_inst_prj}) | _GEN_198;
      queue_prj_ready_5 <=
        (|{io_wake_preg_3 == queue_nxt_5_inst_prj,
           io_wake_preg_2 == queue_nxt_5_inst_prj,
           io_wake_preg_1 == queue_nxt_5_inst_prj,
           io_wake_preg_0 == queue_nxt_5_inst_prj}) | _GEN_201;
      queue_prj_ready_6 <=
        (|{io_wake_preg_3 == queue_nxt_6_inst_prj,
           io_wake_preg_2 == queue_nxt_6_inst_prj,
           io_wake_preg_1 == queue_nxt_6_inst_prj,
           io_wake_preg_0 == queue_nxt_6_inst_prj}) | _GEN_204;
      queue_prj_ready_7 <=
        (|{io_wake_preg_3 == queue_nxt_7_inst_prj,
           io_wake_preg_2 == queue_nxt_7_inst_prj,
           io_wake_preg_1 == queue_nxt_7_inst_prj,
           io_wake_preg_0 == queue_nxt_7_inst_prj}) | _GEN_206;
      queue_prk_ready_0 <=
        (|{io_wake_preg_3 == queue_nxt_inst_prk,
           io_wake_preg_2 == queue_nxt_inst_prk,
           io_wake_preg_1 == queue_nxt_inst_prk,
           io_wake_preg_0 == queue_nxt_inst_prk}) | _GEN_209;
      queue_prk_ready_1 <=
        (|{io_wake_preg_3 == queue_nxt_1_inst_prk,
           io_wake_preg_2 == queue_nxt_1_inst_prk,
           io_wake_preg_1 == queue_nxt_1_inst_prk,
           io_wake_preg_0 == queue_nxt_1_inst_prk}) | _GEN_212;
      queue_prk_ready_2 <=
        (|{io_wake_preg_3 == queue_nxt_2_inst_prk,
           io_wake_preg_2 == queue_nxt_2_inst_prk,
           io_wake_preg_1 == queue_nxt_2_inst_prk,
           io_wake_preg_0 == queue_nxt_2_inst_prk}) | _GEN_215;
      queue_prk_ready_3 <=
        (|{io_wake_preg_3 == queue_nxt_3_inst_prk,
           io_wake_preg_2 == queue_nxt_3_inst_prk,
           io_wake_preg_1 == queue_nxt_3_inst_prk,
           io_wake_preg_0 == queue_nxt_3_inst_prk}) | _GEN_218;
      queue_prk_ready_4 <=
        (|{io_wake_preg_3 == queue_nxt_4_inst_prk,
           io_wake_preg_2 == queue_nxt_4_inst_prk,
           io_wake_preg_1 == queue_nxt_4_inst_prk,
           io_wake_preg_0 == queue_nxt_4_inst_prk}) | _GEN_221;
      queue_prk_ready_5 <=
        (|{io_wake_preg_3 == queue_nxt_5_inst_prk,
           io_wake_preg_2 == queue_nxt_5_inst_prk,
           io_wake_preg_1 == queue_nxt_5_inst_prk,
           io_wake_preg_0 == queue_nxt_5_inst_prk}) | _GEN_224;
      queue_prk_ready_6 <=
        (|{io_wake_preg_3 == queue_nxt_6_inst_prk,
           io_wake_preg_2 == queue_nxt_6_inst_prk,
           io_wake_preg_1 == queue_nxt_6_inst_prk,
           io_wake_preg_0 == queue_nxt_6_inst_prk}) | _GEN_227;
      queue_prk_ready_7 <=
        (|{io_wake_preg_3 == queue_nxt_7_inst_prk,
           io_wake_preg_2 == queue_nxt_7_inst_prk,
           io_wake_preg_1 == queue_nxt_7_inst_prk,
           io_wake_preg_0 == queue_nxt_7_inst_prk}) | _GEN_229;
      if (io_flush) begin
        tail <= 4'h0;
        mask <= 8'h0;
      end
      else if (io_stall_in) begin
        tail <= _tail_pop_T;
        if (is_pop)
          mask <= _mask_pop_T_1;
      end
      else begin
        tail <=
          4'(_tail_pop_T
             + {2'h0, 2'({1'h0, io_insts_valid_0} + {1'h0, io_insts_valid_1})});
        if (&_mask_T_4)
          mask <= {mask_pop[5:0], 2'h3};
        else if (|_mask_T_4)
          mask <= {mask_pop[6:0], 1'h1};
        else if (is_pop)
          mask <= _mask_pop_T_1;
      end
    end
  end // always @(posedge)
  assign io_issue_inst_inst_valid = queue_inst_inst_valid_0;
  assign io_issue_inst_rd_valid = queue_inst_rd_valid_0;
  assign io_issue_inst_imm = queue_inst_imm_0;
  assign io_issue_inst_alu_op = queue_inst_alu_op_0;
  assign io_issue_inst_mem_type = queue_inst_mem_type_0;
  assign io_issue_inst_priv_vec = queue_inst_priv_vec_0;
  assign io_issue_inst_prj = queue_inst_prj_0;
  assign io_issue_inst_prk = queue_inst_prk_0;
  assign io_issue_inst_prd = queue_inst_prd_0;
  assign io_issue_inst_rob_index = queue_inst_rob_index_0;
  assign io_issue_valid = is_pop;
  assign io_full = mask[6];
endmodule


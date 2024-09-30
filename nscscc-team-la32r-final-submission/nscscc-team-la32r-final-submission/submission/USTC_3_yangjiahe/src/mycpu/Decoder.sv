// Generated by CIRCT firtool-1.62.0
module Decoder(
  input         io_inst_FQ_inst_valid,
  input  [31:0] io_inst_FQ_pc,
  input         io_inst_FQ_pred_jump,
  input  [31:0] io_inst_FQ_pred_npc,
  input  [7:0]  io_inst_FQ_exception,
  input  [1:0]  io_inst_FQ_br_cnt,
  input  [31:0] io_inst_FQ_inst,
  output        io_inst_ID_inst_valid,
  output [31:0] io_inst_ID_pc,
  output        io_inst_ID_pred_jump,
  output [31:0] io_inst_ID_pred_npc,
  output [7:0]  io_inst_ID_exception,
  output [1:0]  io_inst_ID_br_cnt,
  output [31:0] io_inst_ID_inst,
  output [4:0]  io_inst_ID_rj,
                io_inst_ID_rk,
                io_inst_ID_rd,
  output        io_inst_ID_rd_valid,
  output [31:0] io_inst_ID_imm,
  output [3:0]  io_inst_ID_alu_op,
  output        io_inst_ID_alu_rs1_sel,
  output [1:0]  io_inst_ID_alu_rs2_sel,
  output [3:0]  io_inst_ID_br_type,
  output [4:0]  io_inst_ID_mem_type,
  output [12:0] io_inst_ID_priv_vec,
  output [2:0]  io_inst_ID_ins_type
);

  wire [4:0]  rd;
  wire        _inst_sel_inst_rdcntvh_w_T = io_inst_FQ_inst[31:26] == 6'h0;
  wire        _inst_sel_inst_rdcntvh_w_T_1 = io_inst_FQ_inst[25:22] == 4'h0;
  wire        _inst_sel_inst_mulh_wu_T_3 = io_inst_FQ_inst[21:20] == 2'h1;
  wire        _inst_sel_inst_rdcntvh_w_T_5 = io_inst_FQ_inst[19:15] == 5'h0;
  wire        inst_sel_inst_add_w =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_mulh_wu_T_3
    & _inst_sel_inst_rdcntvh_w_T_5;
  wire        _inst_sel_inst_div_wu_T_5 = io_inst_FQ_inst[19:15] == 5'h2;
  wire        inst_sel_inst_sub_w =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_mulh_wu_T_3
    & _inst_sel_inst_div_wu_T_5;
  wire        inst_sel_inst_slt =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_mulh_wu_T_3
    & io_inst_FQ_inst[19:15] == 5'h4;
  wire        inst_sel_inst_sltu =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_mulh_wu_T_3
    & io_inst_FQ_inst[19:15] == 5'h5;
  wire        inst_sel_inst_nor =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_mulh_wu_T_3
    & io_inst_FQ_inst[19:15] == 5'h8;
  wire        _inst_sel_inst_srli_w_T_5 = io_inst_FQ_inst[19:15] == 5'h9;
  wire        inst_sel_inst_and =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_mulh_wu_T_3
    & _inst_sel_inst_srli_w_T_5;
  wire        inst_sel_inst_or =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_mulh_wu_T_3
    & io_inst_FQ_inst[19:15] == 5'hA;
  wire        inst_sel_inst_xor =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_mulh_wu_T_3
    & io_inst_FQ_inst[19:15] == 5'hB;
  wire        inst_sel_inst_sll_w =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_mulh_wu_T_3
    & io_inst_FQ_inst[19:15] == 5'hE;
  wire        inst_sel_inst_srl_w =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_mulh_wu_T_3
    & io_inst_FQ_inst[19:15] == 5'hF;
  wire        _inst_sel_inst_tlbfill_T_5 = io_inst_FQ_inst[19:15] == 5'h10;
  wire        inst_sel_inst_sra_w =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_mulh_wu_T_3
    & _inst_sel_inst_tlbfill_T_5;
  wire        inst_sel_inst_mul_w =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_mulh_wu_T_3
    & io_inst_FQ_inst[19:15] == 5'h18;
  wire        inst_sel_inst_mulh_w =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_mulh_wu_T_3
    & io_inst_FQ_inst[19:15] == 5'h19;
  wire        inst_sel_inst_mulh_wu =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_mulh_wu_T_3
    & io_inst_FQ_inst[19:15] == 5'h1A;
  wire        _inst_sel_inst_syscall_T_3 = io_inst_FQ_inst[21:20] == 2'h2;
  wire        inst_sel_inst_div_w =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_syscall_T_3
    & _inst_sel_inst_rdcntvh_w_T_5;
  wire        _inst_sel_inst_slli_w_T_5 = io_inst_FQ_inst[19:15] == 5'h1;
  wire        inst_sel_inst_mod_w =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_syscall_T_3
    & _inst_sel_inst_slli_w_T_5;
  wire        inst_sel_inst_div_wu =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_syscall_T_3
    & _inst_sel_inst_div_wu_T_5;
  wire        inst_sel_inst_mod_wu =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_syscall_T_3
    & io_inst_FQ_inst[19:15] == 5'h3;
  wire        inst_sel_inst_break =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_syscall_T_3
    & io_inst_FQ_inst[19:15] == 5'h14;
  wire        inst_sel_inst_syscall =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_syscall_T_3
    & io_inst_FQ_inst[19:15] == 5'h16;
  wire        _inst_sel_inst_ld_h_T_1 = io_inst_FQ_inst[25:22] == 4'h1;
  wire        _inst_sel_inst_tlbfill_T_3 = io_inst_FQ_inst[21:20] == 2'h0;
  wire        inst_sel_inst_slli_w =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_ld_h_T_1 & _inst_sel_inst_tlbfill_T_3
    & _inst_sel_inst_slli_w_T_5;
  wire        inst_sel_inst_srli_w =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_ld_h_T_1 & _inst_sel_inst_tlbfill_T_3
    & _inst_sel_inst_srli_w_T_5;
  wire        _inst_sel_inst_idle_T_5 = io_inst_FQ_inst[19:15] == 5'h11;
  wire        inst_sel_inst_srai_w =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_ld_h_T_1 & _inst_sel_inst_tlbfill_T_3
    & _inst_sel_inst_idle_T_5;
  wire        _inst_sel_inst_tlbfill_T = io_inst_FQ_inst[31:26] == 6'h1;
  wire        _inst_sel_inst_tlbfill_T_1 = io_inst_FQ_inst[25:22] == 4'h9;
  wire        inst_sel_inst_idle =
    _inst_sel_inst_tlbfill_T & _inst_sel_inst_tlbfill_T_1 & _inst_sel_inst_tlbfill_T_3
    & _inst_sel_inst_idle_T_5;
  wire        inst_sel_inst_invtlb =
    _inst_sel_inst_tlbfill_T & _inst_sel_inst_tlbfill_T_1 & _inst_sel_inst_tlbfill_T_3
    & io_inst_FQ_inst[19:15] == 5'h13;
  wire        _inst_sel_inst_cacop_T_1 = io_inst_FQ_inst[25:22] == 4'h8;
  wire        inst_sel_inst_slti = _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_cacop_T_1;
  wire        inst_sel_inst_sltui =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_tlbfill_T_1;
  wire        inst_sel_inst_addi_w =
    _inst_sel_inst_rdcntvh_w_T & io_inst_FQ_inst[25:22] == 4'hA;
  wire        inst_sel_inst_andi =
    _inst_sel_inst_rdcntvh_w_T & io_inst_FQ_inst[25:22] == 4'hD;
  wire        inst_sel_inst_ori =
    _inst_sel_inst_rdcntvh_w_T & io_inst_FQ_inst[25:22] == 4'hE;
  wire        inst_sel_inst_xori =
    _inst_sel_inst_rdcntvh_w_T & (&(io_inst_FQ_inst[25:22]));
  wire        _inst_sel_inst_ld_hu_T = io_inst_FQ_inst[31:26] == 6'hA;
  wire        inst_sel_inst_ld_b = _inst_sel_inst_ld_hu_T & _inst_sel_inst_rdcntvh_w_T_1;
  wire        inst_sel_inst_ld_h = _inst_sel_inst_ld_hu_T & _inst_sel_inst_ld_h_T_1;
  wire        inst_sel_inst_ld_w =
    _inst_sel_inst_ld_hu_T & io_inst_FQ_inst[25:22] == 4'h2;
  wire        inst_sel_inst_st_b =
    _inst_sel_inst_ld_hu_T & io_inst_FQ_inst[25:22] == 4'h4;
  wire        inst_sel_inst_st_h =
    _inst_sel_inst_ld_hu_T & io_inst_FQ_inst[25:22] == 4'h5;
  wire        inst_sel_inst_st_w =
    _inst_sel_inst_ld_hu_T & io_inst_FQ_inst[25:22] == 4'h6;
  wire        inst_sel_inst_ld_bu = _inst_sel_inst_ld_hu_T & _inst_sel_inst_cacop_T_1;
  wire        inst_sel_inst_ld_hu = _inst_sel_inst_ld_hu_T & _inst_sel_inst_tlbfill_T_1;
  wire        inst_sel_inst_cacop = _inst_sel_inst_tlbfill_T & _inst_sel_inst_cacop_T_1;
  wire        inst_sel_inst_jirl = io_inst_FQ_inst[31:26] == 6'h13;
  wire        inst_sel_inst_b = io_inst_FQ_inst[31:26] == 6'h14;
  wire        inst_sel_inst_bl = io_inst_FQ_inst[31:26] == 6'h15;
  wire        inst_sel_inst_beq = io_inst_FQ_inst[31:26] == 6'h16;
  wire        inst_sel_inst_bne = io_inst_FQ_inst[31:26] == 6'h17;
  wire        inst_sel_inst_blt = io_inst_FQ_inst[31:26] == 6'h18;
  wire        inst_sel_inst_bge = io_inst_FQ_inst[31:26] == 6'h19;
  wire        inst_sel_inst_bltu = io_inst_FQ_inst[31:26] == 6'h1A;
  wire        inst_sel_inst_bgeu = io_inst_FQ_inst[31:26] == 6'h1B;
  wire        inst_sel_inst_lu12i_w =
    io_inst_FQ_inst[31:26] == 6'h5 & ~(io_inst_FQ_inst[25]);
  wire        inst_sel_inst_pcaddu12i =
    io_inst_FQ_inst[31:26] == 6'h7 & ~(io_inst_FQ_inst[25]);
  wire        _inst_sel_inst_tlbfill_T_9 = io_inst_FQ_inst[9:5] == 5'h0;
  wire        _inst_sel_inst_csrwr_T_7 = io_inst_FQ_inst[9:5] == 5'h1;
  wire        inst_sel_inst_csrxchg =
    _inst_sel_inst_tlbfill_T & ~(io_inst_FQ_inst[25]) & ~(io_inst_FQ_inst[24])
    & ~_inst_sel_inst_tlbfill_T_9 & ~_inst_sel_inst_csrwr_T_7;
  wire        _inst_sel_inst_sc_w_T = io_inst_FQ_inst[31:26] == 6'h8;
  wire        inst_sel_inst_ll_w =
    _inst_sel_inst_sc_w_T & ~(io_inst_FQ_inst[25]) & ~(io_inst_FQ_inst[24]);
  wire        inst_sel_inst_sc_w =
    _inst_sel_inst_sc_w_T & ~(io_inst_FQ_inst[25]) & io_inst_FQ_inst[24];
  wire        inst_sel_inst_csrrd =
    _inst_sel_inst_tlbfill_T & ~(io_inst_FQ_inst[25]) & ~(io_inst_FQ_inst[24])
    & _inst_sel_inst_tlbfill_T_9;
  wire        inst_sel_inst_csrwr =
    _inst_sel_inst_tlbfill_T & ~(io_inst_FQ_inst[25]) & ~(io_inst_FQ_inst[24])
    & _inst_sel_inst_csrwr_T_7;
  wire        _inst_sel_inst_rdcntvl_w_T_7 = io_inst_FQ_inst[14:10] == 5'h18;
  wire        _inst_sel_inst_tlbfill_T_11 = io_inst_FQ_inst[4:0] == 5'h0;
  wire        inst_sel_inst_rdcntid_w =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_tlbfill_T_3
    & _inst_sel_inst_rdcntvh_w_T_5 & _inst_sel_inst_rdcntvl_w_T_7
    & _inst_sel_inst_tlbfill_T_11;
  wire        inst_sel_inst_rdcntvl_w =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_tlbfill_T_3
    & _inst_sel_inst_rdcntvh_w_T_5 & _inst_sel_inst_rdcntvl_w_T_7
    & _inst_sel_inst_tlbfill_T_9 & ~_inst_sel_inst_tlbfill_T_11;
  wire        inst_sel_inst_rdcntvh_w =
    _inst_sel_inst_rdcntvh_w_T & _inst_sel_inst_rdcntvh_w_T_1 & _inst_sel_inst_tlbfill_T_3
    & _inst_sel_inst_rdcntvh_w_T_5 & io_inst_FQ_inst[14:10] == 5'h19
    & _inst_sel_inst_tlbfill_T_9;
  wire        imm_sel_imm_ERA =
    _inst_sel_inst_tlbfill_T & _inst_sel_inst_tlbfill_T_1 & _inst_sel_inst_tlbfill_T_3
    & _inst_sel_inst_tlbfill_T_5 & io_inst_FQ_inst[14:10] == 5'hE
    & _inst_sel_inst_tlbfill_T_9 & _inst_sel_inst_tlbfill_T_11;
  wire        inst_sel_inst_tlbsrch =
    _inst_sel_inst_tlbfill_T & _inst_sel_inst_tlbfill_T_1 & _inst_sel_inst_tlbfill_T_3
    & _inst_sel_inst_tlbfill_T_5 & io_inst_FQ_inst[14:10] == 5'hA
    & _inst_sel_inst_tlbfill_T_9 & _inst_sel_inst_tlbfill_T_11;
  wire        inst_sel_inst_tlbrd =
    _inst_sel_inst_tlbfill_T & _inst_sel_inst_tlbfill_T_1 & _inst_sel_inst_tlbfill_T_3
    & _inst_sel_inst_tlbfill_T_5 & io_inst_FQ_inst[14:10] == 5'hB
    & _inst_sel_inst_tlbfill_T_9 & _inst_sel_inst_tlbfill_T_11;
  wire        inst_sel_inst_tlbwr =
    _inst_sel_inst_tlbfill_T & _inst_sel_inst_tlbfill_T_1 & _inst_sel_inst_tlbfill_T_3
    & _inst_sel_inst_tlbfill_T_5 & io_inst_FQ_inst[14:10] == 5'hC
    & _inst_sel_inst_tlbfill_T_9 & _inst_sel_inst_tlbfill_T_11;
  wire        inst_sel_inst_tlbfill =
    _inst_sel_inst_tlbfill_T & _inst_sel_inst_tlbfill_T_1 & _inst_sel_inst_tlbfill_T_3
    & _inst_sel_inst_tlbfill_T_5 & io_inst_FQ_inst[14:10] == 5'hD
    & _inst_sel_inst_tlbfill_T_9 & _inst_sel_inst_tlbfill_T_11;
  wire        _ins_type_T = inst_sel_inst_add_w | inst_sel_inst_sub_w;
  wire [4:0]  _rk_T_2 =
    inst_sel_inst_csrwr | inst_sel_inst_csrxchg | imm_sel_imm_ERA | inst_sel_inst_idle
    | inst_sel_inst_sc_w | inst_sel_inst_st_b | inst_sel_inst_st_h | inst_sel_inst_st_w
    | inst_sel_inst_beq | inst_sel_inst_bne | inst_sel_inst_blt | inst_sel_inst_bge
    | inst_sel_inst_bltu | inst_sel_inst_bgeu
      ? io_inst_FQ_inst[4:0]
      : io_inst_FQ_inst[14:10];
  wire [4:0]  _rd_T_2 = inst_sel_inst_bl ? 5'h1 : io_inst_FQ_inst[4:0];
  assign rd = inst_sel_inst_rdcntid_w ? io_inst_FQ_inst[9:5] : _rd_T_2;
  wire        imm_sel_imm_14S = inst_sel_inst_ll_w | inst_sel_inst_sc_w;
  wire [31:0] _imm_T_54 =
    inst_sel_inst_slli_w | inst_sel_inst_srli_w | inst_sel_inst_srai_w
      ? {27'h0, io_inst_FQ_inst[14:10]}
      : 32'h0;
  wire [31:0] _imm_T_44 =
    inst_sel_inst_andi | inst_sel_inst_ori | inst_sel_inst_xori
      ? {20'h0, io_inst_FQ_inst[21:10]}
      : 32'h0;
  wire [31:0] _imm_T_45 =
    inst_sel_inst_slti | inst_sel_inst_sltui | inst_sel_inst_addi_w | inst_sel_inst_ld_b
    | inst_sel_inst_ld_h | inst_sel_inst_ld_w | inst_sel_inst_st_b | inst_sel_inst_st_h
    | inst_sel_inst_st_w | inst_sel_inst_ld_bu | inst_sel_inst_ld_hu
      ? {{20{io_inst_FQ_inst[21]}}, io_inst_FQ_inst[21:10]}
      : 32'h0;
  wire [31:0] _imm_T_46 =
    imm_sel_imm_14S ? {{18{io_inst_FQ_inst[21]}}, io_inst_FQ_inst[21:10], 2'h0} : 32'h0;
  wire [31:0] _imm_T_47 =
    inst_sel_inst_jirl | inst_sel_inst_beq | inst_sel_inst_bne | inst_sel_inst_blt
    | inst_sel_inst_bge | inst_sel_inst_bltu | inst_sel_inst_bgeu
      ? {{14{io_inst_FQ_inst[25]}}, io_inst_FQ_inst[25:10], 2'h0}
      : 32'h0;
  wire [31:0] _imm_T_48 =
    inst_sel_inst_lu12i_w | inst_sel_inst_pcaddu12i
      ? {io_inst_FQ_inst[24:5], 12'h0}
      : 32'h0;
  wire [31:0] _imm_T_49 =
    inst_sel_inst_b | inst_sel_inst_bl
      ? {{4{io_inst_FQ_inst[9]}}, io_inst_FQ_inst[9:0], io_inst_FQ_inst[25:10], 2'h0}
      : 32'h0;
  wire [31:0] _imm_T_50 =
    inst_sel_inst_csrrd | inst_sel_inst_csrwr | inst_sel_inst_csrxchg
      ? {18'h0, io_inst_FQ_inst[23:10]}
      : 32'h0;
  wire [31:0] _imm_T_52 = imm_sel_imm_ERA ? 32'h6 : 32'h0;
  wire [31:0] _imm_T_53 =
    inst_sel_inst_cacop | inst_sel_inst_invtlb
      ? {{15{io_inst_FQ_inst[21]}}, io_inst_FQ_inst[21:10], io_inst_FQ_inst[4:0]}
      : 32'h0;
  wire        mem_valid =
    inst_sel_inst_ld_b | inst_sel_inst_ld_h | inst_sel_inst_ld_w | inst_sel_inst_st_b
    | inst_sel_inst_st_h | inst_sel_inst_st_w | inst_sel_inst_ld_bu | inst_sel_inst_ld_hu
    | inst_sel_inst_ll_w | inst_sel_inst_sc_w;
  wire [1:0]  _mem_type_T_1 = mem_valid ? io_inst_FQ_inst[25:24] : 2'h0;
  wire [1:0]  _mem_type_T_5 = mem_valid ? io_inst_FQ_inst[23:22] : 2'h0;
  wire [1:0]  _mem_type_T_6 = imm_sel_imm_14S ? 2'h2 : _mem_type_T_5;
  wire        alu_op_sel_5 = inst_sel_inst_nor | inst_sel_inst_mod_w;
  wire        alu_op_sel_6 = inst_sel_inst_or | inst_sel_inst_ori | inst_sel_inst_div_wu;
  wire        alu_op_sel_7 =
    inst_sel_inst_xor | inst_sel_inst_xori | inst_sel_inst_mod_wu;
  wire        alu_op_sel_9 = inst_sel_inst_srl_w | inst_sel_inst_srli_w;
  wire        alu_op_sel_10 = inst_sel_inst_sra_w | inst_sel_inst_srai_w;
  wire [2:0]  _alu_op_T_4 =
    {alu_op_sel_7, alu_op_sel_6, alu_op_sel_5}
    | {inst_sel_inst_sltu | inst_sel_inst_sltui,
       alu_op_sel_10 | inst_sel_inst_slt | inst_sel_inst_slti | inst_sel_inst_mulh_wu,
       alu_op_sel_9 | inst_sel_inst_sub_w | inst_sel_inst_mulh_w};
  wire        _alu_rs2_sel_T = inst_sel_inst_rdcntvl_w | inst_sel_inst_rdcntvh_w;
  wire [7:0]  _GEN = inst_sel_inst_syscall ? 8'h8B : 8'h0;
  wire [7:0]  _GEN_0 = inst_sel_inst_break ? 8'h8C : _GEN;
  wire [7:0]  exception =
    {inst_sel_inst_add_w,
     inst_sel_inst_sub_w,
     inst_sel_inst_slt,
     inst_sel_inst_sltu,
     inst_sel_inst_nor,
     inst_sel_inst_and,
     inst_sel_inst_or,
     inst_sel_inst_xor,
     inst_sel_inst_sll_w,
     inst_sel_inst_srl_w,
     inst_sel_inst_sra_w,
     inst_sel_inst_mul_w,
     inst_sel_inst_mulh_w,
     inst_sel_inst_mulh_wu,
     inst_sel_inst_div_w,
     inst_sel_inst_mod_w,
     inst_sel_inst_div_wu,
     inst_sel_inst_mod_wu,
     inst_sel_inst_break,
     inst_sel_inst_syscall,
     inst_sel_inst_slli_w,
     inst_sel_inst_srli_w,
     inst_sel_inst_srai_w,
     inst_sel_inst_idle,
     inst_sel_inst_invtlb,
     inst_sel_inst_slti,
     inst_sel_inst_sltui,
     inst_sel_inst_addi_w,
     inst_sel_inst_andi,
     inst_sel_inst_ori,
     inst_sel_inst_xori,
     inst_sel_inst_ld_b,
     inst_sel_inst_ld_h,
     inst_sel_inst_ld_w,
     inst_sel_inst_st_b,
     inst_sel_inst_st_h,
     inst_sel_inst_st_w,
     inst_sel_inst_ld_bu,
     inst_sel_inst_ld_hu,
     inst_sel_inst_cacop,
     inst_sel_inst_jirl,
     inst_sel_inst_b,
     inst_sel_inst_bl,
     inst_sel_inst_beq,
     inst_sel_inst_bne,
     inst_sel_inst_blt,
     inst_sel_inst_bge,
     inst_sel_inst_bltu,
     inst_sel_inst_bgeu,
     inst_sel_inst_lu12i_w,
     inst_sel_inst_pcaddu12i,
     inst_sel_inst_csrxchg,
     inst_sel_inst_ll_w,
     inst_sel_inst_sc_w,
     inst_sel_inst_csrrd,
     inst_sel_inst_csrwr,
     inst_sel_inst_rdcntid_w,
     inst_sel_inst_rdcntvl_w,
     inst_sel_inst_rdcntvh_w,
     imm_sel_imm_ERA,
     inst_sel_inst_tlbsrch,
     inst_sel_inst_tlbrd,
     inst_sel_inst_tlbwr,
     inst_sel_inst_tlbfill} == 64'h0
      ? 8'h8D
      : _GEN_0;
  assign io_inst_ID_inst_valid = io_inst_FQ_inst_valid;
  assign io_inst_ID_pc = io_inst_FQ_pc;
  assign io_inst_ID_pred_jump = io_inst_FQ_pred_jump;
  assign io_inst_ID_pred_npc = io_inst_FQ_pred_npc;
  assign io_inst_ID_exception =
    (|io_inst_FQ_exception) ? io_inst_FQ_exception : exception;
  assign io_inst_ID_br_cnt = io_inst_FQ_br_cnt;
  assign io_inst_ID_inst = io_inst_FQ_inst;
  assign io_inst_ID_rj =
    _ins_type_T | inst_sel_inst_slt | inst_sel_inst_sltu | inst_sel_inst_nor
    | inst_sel_inst_and | inst_sel_inst_or | inst_sel_inst_xor | inst_sel_inst_sll_w
    | inst_sel_inst_srl_w | inst_sel_inst_sra_w | inst_sel_inst_mul_w
    | inst_sel_inst_mulh_w | inst_sel_inst_mulh_wu | inst_sel_inst_div_w
    | inst_sel_inst_mod_w | inst_sel_inst_div_wu | inst_sel_inst_mod_wu
    | inst_sel_inst_slli_w | inst_sel_inst_srli_w | inst_sel_inst_srai_w
    | inst_sel_inst_slti | inst_sel_inst_sltui | inst_sel_inst_addi_w | inst_sel_inst_andi
    | inst_sel_inst_ori | inst_sel_inst_xori | inst_sel_inst_csrxchg | inst_sel_inst_cacop
    | inst_sel_inst_tlbsrch | inst_sel_inst_tlbrd | inst_sel_inst_tlbwr
    | inst_sel_inst_tlbfill | inst_sel_inst_invtlb | inst_sel_inst_ll_w
    | inst_sel_inst_sc_w | inst_sel_inst_ld_b | inst_sel_inst_ld_h | inst_sel_inst_ld_w
    | inst_sel_inst_st_b | inst_sel_inst_st_h | inst_sel_inst_st_w | inst_sel_inst_ld_bu
    | inst_sel_inst_ld_hu | inst_sel_inst_jirl | inst_sel_inst_beq | inst_sel_inst_bne
    | inst_sel_inst_blt | inst_sel_inst_bge | inst_sel_inst_bltu | inst_sel_inst_bgeu
      ? io_inst_FQ_inst[9:5]
      : 5'h0;
  assign io_inst_ID_rk =
    _ins_type_T | inst_sel_inst_slt | inst_sel_inst_sltu | inst_sel_inst_nor
    | inst_sel_inst_and | inst_sel_inst_or | inst_sel_inst_xor | inst_sel_inst_sll_w
    | inst_sel_inst_srl_w | inst_sel_inst_sra_w | inst_sel_inst_mul_w
    | inst_sel_inst_mulh_w | inst_sel_inst_mulh_wu | inst_sel_inst_div_w
    | inst_sel_inst_mod_w | inst_sel_inst_div_wu | inst_sel_inst_mod_wu
    | inst_sel_inst_csrwr | inst_sel_inst_csrxchg | inst_sel_inst_invtlb
    | inst_sel_inst_sc_w | inst_sel_inst_st_b | inst_sel_inst_st_h | inst_sel_inst_st_w
    | inst_sel_inst_beq | inst_sel_inst_bne | inst_sel_inst_blt | inst_sel_inst_bge
    | inst_sel_inst_bltu | inst_sel_inst_bgeu
      ? _rk_T_2
      : 5'h0;
  assign io_inst_ID_rd = rd;
  assign io_inst_ID_rd_valid =
    (inst_sel_inst_rdcntid_w | inst_sel_inst_rdcntvl_w | inst_sel_inst_rdcntvh_w
     | inst_sel_inst_add_w | inst_sel_inst_sub_w | inst_sel_inst_slt | inst_sel_inst_sltu
     | inst_sel_inst_nor | inst_sel_inst_and | inst_sel_inst_or | inst_sel_inst_xor
     | inst_sel_inst_sll_w | inst_sel_inst_srl_w | inst_sel_inst_sra_w
     | inst_sel_inst_mul_w | inst_sel_inst_mulh_w | inst_sel_inst_mulh_wu
     | inst_sel_inst_div_w | inst_sel_inst_mod_w | inst_sel_inst_div_wu
     | inst_sel_inst_mod_wu | inst_sel_inst_slli_w | inst_sel_inst_srli_w
     | inst_sel_inst_srai_w | inst_sel_inst_slti | inst_sel_inst_sltui
     | inst_sel_inst_addi_w | inst_sel_inst_andi | inst_sel_inst_ori | inst_sel_inst_xori
     | inst_sel_inst_csrrd | inst_sel_inst_csrwr | inst_sel_inst_csrxchg
     | inst_sel_inst_tlbsrch | inst_sel_inst_tlbrd | inst_sel_inst_tlbwr
     | inst_sel_inst_tlbfill | inst_sel_inst_lu12i_w | inst_sel_inst_pcaddu12i
     | inst_sel_inst_ll_w | inst_sel_inst_sc_w | inst_sel_inst_ld_b | inst_sel_inst_ld_h
     | inst_sel_inst_ld_w | inst_sel_inst_ld_bu | inst_sel_inst_ld_hu | inst_sel_inst_jirl
     | inst_sel_inst_bl) & (|rd);
  assign io_inst_ID_imm =
    _imm_T_54 | _imm_T_44 | _imm_T_45 | _imm_T_46 | _imm_T_47 | _imm_T_48 | _imm_T_49
    | _imm_T_50 | {25'h0, inst_sel_inst_tlbsrch | inst_sel_inst_rdcntid_w, 6'h0}
    | _imm_T_52 | _imm_T_53;
  assign io_inst_ID_alu_op =
    {|{alu_op_sel_10, alu_op_sel_9, inst_sel_inst_sll_w | inst_sel_inst_slli_w},
     |{alu_op_sel_7,
       alu_op_sel_6,
       alu_op_sel_5,
       inst_sel_inst_and | inst_sel_inst_andi | inst_sel_inst_div_w},
     |(_alu_op_T_4[2:1]),
     _alu_op_T_4[2] | _alu_op_T_4[0]};
  assign io_inst_ID_alu_rs1_sel =
    inst_sel_inst_pcaddu12i | inst_sel_inst_jirl | inst_sel_inst_bl;
  assign io_inst_ID_alu_rs2_sel =
    {_alu_rs2_sel_T | inst_sel_inst_jirl | inst_sel_inst_bl,
     inst_sel_inst_rdcntvl_w | inst_sel_inst_add_w | inst_sel_inst_sub_w
       | inst_sel_inst_slt | inst_sel_inst_sltu | inst_sel_inst_nor | inst_sel_inst_and
       | inst_sel_inst_or | inst_sel_inst_xor | inst_sel_inst_sll_w | inst_sel_inst_srl_w
       | inst_sel_inst_sra_w | inst_sel_inst_mul_w | inst_sel_inst_mulh_w
       | inst_sel_inst_mulh_wu | inst_sel_inst_div_w | inst_sel_inst_mod_w
       | inst_sel_inst_div_wu | inst_sel_inst_mod_wu};
  assign io_inst_ID_br_type =
    inst_sel_inst_beq | inst_sel_inst_bne | inst_sel_inst_blt | inst_sel_inst_bge
    | inst_sel_inst_bltu | inst_sel_inst_bgeu | inst_sel_inst_b | inst_sel_inst_bl
    | inst_sel_inst_jirl
      ? io_inst_FQ_inst[29:26]
      : 4'h0;
  assign io_inst_ID_mem_type = {mem_valid, _mem_type_T_1, _mem_type_T_6};
  assign io_inst_ID_priv_vec =
    {inst_sel_inst_sc_w,
     inst_sel_inst_ll_w,
     inst_sel_inst_cacop,
     inst_sel_inst_idle,
     inst_sel_inst_invtlb,
     inst_sel_inst_tlbsrch,
     inst_sel_inst_tlbfill,
     inst_sel_inst_tlbwr,
     inst_sel_inst_tlbrd,
     imm_sel_imm_ERA,
     inst_sel_inst_csrxchg,
     inst_sel_inst_csrwr,
     inst_sel_inst_rdcntid_w | inst_sel_inst_csrrd
       | (|{inst_sel_inst_sc_w,
            inst_sel_inst_ll_w,
            inst_sel_inst_cacop,
            inst_sel_inst_idle,
            inst_sel_inst_invtlb,
            inst_sel_inst_tlbsrch,
            inst_sel_inst_tlbfill,
            inst_sel_inst_tlbwr,
            inst_sel_inst_tlbrd,
            imm_sel_imm_ERA,
            inst_sel_inst_csrxchg,
            inst_sel_inst_csrwr})};
  assign io_inst_ID_ins_type =
    (|io_inst_FQ_exception)
      ? 3'h0
      : {_ins_type_T | inst_sel_inst_slt | inst_sel_inst_sltu | inst_sel_inst_nor
           | inst_sel_inst_and | inst_sel_inst_or | inst_sel_inst_xor
           | inst_sel_inst_sll_w | inst_sel_inst_srl_w | inst_sel_inst_sra_w
           | inst_sel_inst_slli_w | inst_sel_inst_srli_w | inst_sel_inst_srai_w
           | inst_sel_inst_slti | inst_sel_inst_sltui | inst_sel_inst_addi_w
           | inst_sel_inst_andi | inst_sel_inst_ori | inst_sel_inst_xori
           | inst_sel_inst_lu12i_w | inst_sel_inst_pcaddu12i,
         inst_sel_inst_cacop | inst_sel_inst_ll_w | inst_sel_inst_sc_w
           | inst_sel_inst_ld_b | inst_sel_inst_ld_h | inst_sel_inst_ld_w
           | inst_sel_inst_st_b | inst_sel_inst_st_h | inst_sel_inst_st_w
           | inst_sel_inst_ld_bu | inst_sel_inst_ld_hu | inst_sel_inst_jirl
           | inst_sel_inst_b | inst_sel_inst_bl | inst_sel_inst_beq | inst_sel_inst_bne
           | inst_sel_inst_blt | inst_sel_inst_bge | inst_sel_inst_bltu
           | inst_sel_inst_bgeu,
         _alu_rs2_sel_T | inst_sel_inst_cacop | inst_sel_inst_ll_w | inst_sel_inst_sc_w
           | inst_sel_inst_ld_b | inst_sel_inst_ld_h | inst_sel_inst_ld_w
           | inst_sel_inst_st_b | inst_sel_inst_st_h | inst_sel_inst_st_w
           | inst_sel_inst_ld_bu | inst_sel_inst_ld_hu};
endmodule


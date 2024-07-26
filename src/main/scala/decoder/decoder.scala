import chisel3._
import chisel3.util._
import InstPacks._

class inst_sel extends Bundle {
    val inst_add_w      = Bool()
    val inst_sub_w      = Bool()
    val inst_slt        = Bool()
    val inst_sltu       = Bool()
    val inst_nor        = Bool()
    val inst_and        = Bool()
    val inst_or         = Bool()
    val inst_xor        = Bool()
    //val inst_orn        = Bool()
    //val inst_andn       = Bool()
    val inst_sll_w      = Bool()
    val inst_srl_w      = Bool()
    val inst_sra_w      = Bool()
    val inst_mul_w      = Bool()
    val inst_mulh_w     = Bool()
    val inst_mulh_wu    = Bool()
    val inst_div_w      = Bool()
    val inst_mod_w      = Bool()
    val inst_div_wu     = Bool()
    val inst_mod_wu     = Bool()
    val inst_break      = Bool()
    val inst_syscall    = Bool()
    val inst_slli_w     = Bool()
    val inst_srli_w     = Bool()
    val inst_srai_w     = Bool()
    val inst_idle       = Bool()
    val inst_invtlb     = Bool()
    //val inst_dbar       = Bool()
    //val inst_ibar       = Bool()
    val inst_slti       = Bool()
    val inst_sltui      = Bool()
    val inst_addi_w     = Bool()
    val inst_andi       = Bool()
    val inst_ori        = Bool()
    val inst_xori       = Bool()
    val inst_ld_b       = Bool()
    val inst_ld_h       = Bool()
    val inst_ld_w       = Bool()
    val inst_st_b       = Bool()
    val inst_st_h       = Bool()
    val inst_st_w       = Bool()
    val inst_ld_bu      = Bool()
    val inst_ld_hu      = Bool()
    val inst_cacop      = Bool()
    //val inst_preld      = Bool()
    val inst_jirl       = Bool()
    val inst_b          = Bool()
    val inst_bl         = Bool()
    val inst_beq        = Bool()
    val inst_bne        = Bool()
    val inst_blt        = Bool()
    val inst_bge        = Bool()
    val inst_bltu       = Bool()
    val inst_bgeu       = Bool()
    val inst_lu12i_w    = Bool()
    //val inst_pcaddi     = Bool()
    val inst_pcaddu12i  = Bool()
    val inst_csrxchg    = Bool()
    val inst_ll_w       = Bool()
    val inst_sc_w       = Bool()
    val inst_csrrd      = Bool()
    val inst_csrwr      = Bool()
    val inst_rdcntid_w  = Bool()
    val inst_rdcntvl_w  = Bool()
    val inst_rdcntvh_w  = Bool()
    val inst_ertn       = Bool()
    val inst_tlbsrch    = Bool()
    val inst_tlbrd      = Bool()
    val inst_tlbwr      = Bool()
    val inst_tlbfill    = Bool()
}

class imm_sel extends Bundle {
    val imm_00U = Bool()
    val imm_05U = Bool()
    val imm_12U = Bool()
    val imm_12S = Bool()
    val imm_14S = Bool()
    val imm_16S = Bool()
    val imm_20S = Bool()
    val imm_26S = Bool()
    val imm_CSR = Bool()
    val imm_TID = Bool()
    val imm_ERA = Bool()
    val imm_COP = Bool()
}

class Decoder extends Module {
    val io = IO(new Bundle{
        val inst_FQ = Input(new pack_PD)
        val inst_ID = Output(new pack_ID)
    })
    val rj          = Wire(UInt(5.W))
    val rk          = Wire(UInt(5.W))
    val rd          = Wire(UInt(5.W))
    val rd_valid    = Wire(Bool())
    val imm         = Wire(UInt(32.W))
    val alu_op      = Wire(UInt(4.W))
    val alu_rs1_sel = Wire(UInt(1.W))
    val alu_rs2_sel = Wire(UInt(2.W))
    val br_type     = Wire(UInt(4.W))
    val mem_type    = Wire(UInt(5.W))
    val priv_vec    = Wire(UInt(13.W))
    val exception   = Wire(UInt(8.W))
    val ins_type    = Wire(UInt(3.W))

    val inst = io.inst_FQ.inst
    val inst_ID = pack_ID(io.inst_FQ)
    //把指令码和寄存器编号转换为独热编码
    val inst_31_26 = Wire(UInt(64.W))
    val inst_25_22 = Wire(UInt(16.W))
    val inst_21_20 = Wire(UInt(4.W))
    val inst_19_15 = Wire(UInt(32.W))
    val inst_14_10 = Wire(UInt(32.W))
    val inst_9_5 = Wire(UInt(32.W))
    val inst_4_0 = Wire(UInt(32.W))

    inst_31_26 := UIntToOH(inst(31,26))
    inst_25_22 := UIntToOH(inst(25,22))
    inst_21_20 := UIntToOH(inst(21,20))
    inst_19_15 := UIntToOH(inst(19,15))
    inst_14_10 := UIntToOH(inst(14,10))
    inst_9_5 := UIntToOH(inst(9,5))
    inst_4_0 := UIntToOH(inst(4,0))

    //给inst_sel赋值，确定指令类型
    val inst_sel = Wire(new inst_sel)

    inst_sel.inst_add_w      := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x1) & inst_19_15(0x00)
    inst_sel.inst_sub_w      := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x1) & inst_19_15(0x02)
    inst_sel.inst_slt        := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x1) & inst_19_15(0x04)
    inst_sel.inst_sltu       := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x1) & inst_19_15(0x05)
    inst_sel.inst_nor        := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x1) & inst_19_15(0x08)
    inst_sel.inst_and        := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x1) & inst_19_15(0x09)
    inst_sel.inst_or         := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x1) & inst_19_15(0x0a)
    inst_sel.inst_xor        := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x1) & inst_19_15(0x0b)
    //inst_sel.inst_orn        = inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x1) & inst_19_15(0x0c)
    //inst_sel.inst_andn       = inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x1) & inst_19_15(0x0d)
    inst_sel.inst_sll_w      := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x1) & inst_19_15(0x0e)
    inst_sel.inst_srl_w      := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x1) & inst_19_15(0x0f)
    inst_sel.inst_sra_w      := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x1) & inst_19_15(0x10)
    inst_sel.inst_mul_w      := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x1) & inst_19_15(0x18)
    inst_sel.inst_mulh_w     := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x1) & inst_19_15(0x19)
    inst_sel.inst_mulh_wu    := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x1) & inst_19_15(0x1a)
    inst_sel.inst_div_w      := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x2) & inst_19_15(0x00)
    inst_sel.inst_mod_w      := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x2) & inst_19_15(0x01)
    inst_sel.inst_div_wu     := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x2) & inst_19_15(0x02)
    inst_sel.inst_mod_wu     := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x2) & inst_19_15(0x03)
    inst_sel.inst_break      := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x2) & inst_19_15(0x14)
    inst_sel.inst_syscall    := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x2) & inst_19_15(0x16)
    inst_sel.inst_slli_w     := inst_31_26(0x00) & inst_25_22(0x1) & inst_21_20(0x0) & inst_19_15(0x01)
    inst_sel.inst_srli_w     := inst_31_26(0x00) & inst_25_22(0x1) & inst_21_20(0x0) & inst_19_15(0x09)
    inst_sel.inst_srai_w     := inst_31_26(0x00) & inst_25_22(0x1) & inst_21_20(0x0) & inst_19_15(0x11)
    inst_sel.inst_idle       := inst_31_26(0x01) & inst_25_22(0x9) & inst_21_20(0x0) & inst_19_15(0x11)
    inst_sel.inst_invtlb     := inst_31_26(0x01) & inst_25_22(0x9) & inst_21_20(0x0) & inst_19_15(0x13)
    //inst_sel.inst_dbar       = inst_31_26(0x0e) & inst_25_22(0x1) & inst_21_20(0x3) & inst_19_15(0x04)
    //inst_sel.inst_ibar       = inst_31_26(0x0e) & inst_25_22(0x1) & inst_21_20(0x3) & inst_19_15(0x05)
    inst_sel.inst_slti       := inst_31_26(0x00) & inst_25_22(0x8)
    inst_sel.inst_sltui      := inst_31_26(0x00) & inst_25_22(0x9)
    inst_sel.inst_addi_w     := inst_31_26(0x00) & inst_25_22(0xa)
    inst_sel.inst_andi       := inst_31_26(0x00) & inst_25_22(0xd)
    inst_sel.inst_ori        := inst_31_26(0x00) & inst_25_22(0xe)
    inst_sel.inst_xori       := inst_31_26(0x00) & inst_25_22(0xf)
    inst_sel.inst_ld_b       := inst_31_26(0x0a) & inst_25_22(0x0)
    inst_sel.inst_ld_h       := inst_31_26(0x0a) & inst_25_22(0x1)
    inst_sel.inst_ld_w       := inst_31_26(0x0a) & inst_25_22(0x2)
    inst_sel.inst_st_b       := inst_31_26(0x0a) & inst_25_22(0x4)
    inst_sel.inst_st_h       := inst_31_26(0x0a) & inst_25_22(0x5)
    inst_sel.inst_st_w       := inst_31_26(0x0a) & inst_25_22(0x6)
    inst_sel.inst_ld_bu      := inst_31_26(0x0a) & inst_25_22(0x8)
    inst_sel.inst_ld_hu      := inst_31_26(0x0a) & inst_25_22(0x9)
    inst_sel.inst_cacop      := inst_31_26(0x01) & inst_25_22(0x8)
    //inst_sel.inst_preld      = inst_31_26(0x0a) & inst_25_22(0xb)
    inst_sel.inst_jirl       := inst_31_26(0x13)
    inst_sel.inst_b          := inst_31_26(0x14)
    inst_sel.inst_bl         := inst_31_26(0x15)
    inst_sel.inst_beq        := inst_31_26(0x16)
    inst_sel.inst_bne        := inst_31_26(0x17)
    inst_sel.inst_blt        := inst_31_26(0x18)
    inst_sel.inst_bge        := inst_31_26(0x19)
    inst_sel.inst_bltu       := inst_31_26(0x1a)
    inst_sel.inst_bgeu       := inst_31_26(0x1b)
    inst_sel.inst_lu12i_w    := inst_31_26(0x05) & ~inst(25)
    //inst_sel.inst_pcaddi     = inst_31_26(0x06) & ~inst(25)
    inst_sel.inst_pcaddu12i  := inst_31_26(0x07) & ~inst(25)
    inst_sel.inst_csrxchg    := inst_31_26(0x01) & ~inst(25) & ~inst(24) & (~inst_9_5(0x00) & ~inst_9_5(0x01))  //rj != 0,1
    inst_sel.inst_ll_w       := inst_31_26(0x08) & ~inst(25) & ~inst(24)
    inst_sel.inst_sc_w       := inst_31_26(0x08) & ~inst(25) &  inst(24)
    inst_sel.inst_csrrd      := inst_31_26(0x01) & ~inst(25) & ~inst(24) & inst_9_5(0x00)
    inst_sel.inst_csrwr      := inst_31_26(0x01) & ~inst(25) & ~inst(24) & inst_9_5(0x01)
    inst_sel.inst_rdcntid_w  := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x0) & inst_19_15(0x00) & inst_14_10(0x18) & inst_4_0(0x00)
    inst_sel.inst_rdcntvl_w  := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x0) & inst_19_15(0x00) & inst_14_10(0x18) & inst_9_5(0x00) & !inst_4_0(0x00)
    inst_sel.inst_rdcntvh_w  := inst_31_26(0x00) & inst_25_22(0x0) & inst_21_20(0x0) & inst_19_15(0x00) & inst_14_10(0x19) & inst_9_5(0x00)
    inst_sel.inst_ertn       := inst_31_26(0x01) & inst_25_22(0x9) & inst_21_20(0x0) & inst_19_15(0x10) & inst_14_10(0x0e) & inst_9_5(0x00) & inst_4_0(0x00)
    inst_sel.inst_tlbsrch    := inst_31_26(0x01) & inst_25_22(0x9) & inst_21_20(0x0) & inst_19_15(0x10) & inst_14_10(0x0a) & inst_9_5(0x00) & inst_4_0(0x00)
    inst_sel.inst_tlbrd      := inst_31_26(0x01) & inst_25_22(0x9) & inst_21_20(0x0) & inst_19_15(0x10) & inst_14_10(0x0b) & inst_9_5(0x00) & inst_4_0(0x00)
    inst_sel.inst_tlbwr      := inst_31_26(0x01) & inst_25_22(0x9) & inst_21_20(0x0) & inst_19_15(0x10) & inst_14_10(0x0c) & inst_9_5(0x00) & inst_4_0(0x00)
    inst_sel.inst_tlbfill    := inst_31_26(0x01) & inst_25_22(0x9) & inst_21_20(0x0) & inst_19_15(0x10) & inst_14_10(0x0d) & inst_9_5(0x00) & inst_4_0(0x00)

    //rj赋值
    val rj_sel =Wire(Bool())
    rj_sel :=   inst_sel.inst_add_w    |
                inst_sel.inst_sub_w    |
                inst_sel.inst_slt      |
                inst_sel.inst_sltu     |
                inst_sel.inst_nor      |
                inst_sel.inst_and      |
                inst_sel.inst_or       |
                inst_sel.inst_xor      |
                inst_sel.inst_sll_w    |
                inst_sel.inst_srl_w    |
                inst_sel.inst_sra_w    |
                inst_sel.inst_mul_w    |
                inst_sel.inst_mulh_w   |
                inst_sel.inst_mulh_wu  |
                inst_sel.inst_div_w    |
                inst_sel.inst_mod_w    |
                inst_sel.inst_div_wu   |
                inst_sel.inst_mod_wu   |
                inst_sel.inst_slli_w   |
                inst_sel.inst_srli_w   |
                inst_sel.inst_srai_w   |
                inst_sel.inst_slti     |
                inst_sel.inst_sltui    |
                inst_sel.inst_addi_w   |
                inst_sel.inst_andi     |
                inst_sel.inst_ori      |
                inst_sel.inst_xori     |
                inst_sel.inst_csrxchg  |
                inst_sel.inst_cacop    |
                inst_sel.inst_tlbsrch  |
                inst_sel.inst_tlbrd    |
                inst_sel.inst_tlbwr    |
                inst_sel.inst_tlbfill  |
                inst_sel.inst_invtlb   |
                inst_sel.inst_ll_w     |
                inst_sel.inst_sc_w     |
                inst_sel.inst_ld_b     |
                inst_sel.inst_ld_h     |
                inst_sel.inst_ld_w     |
                inst_sel.inst_st_b     |
                inst_sel.inst_st_h     |
                inst_sel.inst_st_w     |
                inst_sel.inst_ld_bu    |
                inst_sel.inst_ld_hu    |
                inst_sel.inst_jirl     |
                inst_sel.inst_beq      |
                inst_sel.inst_bne      |
                inst_sel.inst_blt      |
                inst_sel.inst_bge      |
                inst_sel.inst_bltu     |
                inst_sel.inst_bgeu     
    rj := Mux(rj_sel, inst(19,15), 0.U)

    //rk赋值
    val rk_valid = Wire(Bool())
    val rk_sel = Wire(Bool())
    rk_valid := inst_sel.inst_add_w    |
                inst_sel.inst_sub_w    |
                inst_sel.inst_slt      |
                inst_sel.inst_sltu     |
                inst_sel.inst_nor      |
                inst_sel.inst_and      |
                inst_sel.inst_or       |
                inst_sel.inst_xor      |
                inst_sel.inst_sll_w    |
                inst_sel.inst_srl_w    |
                inst_sel.inst_sra_w    |
                inst_sel.inst_mul_w    |
                inst_sel.inst_mulh_w   |
                inst_sel.inst_mulh_wu  |
                inst_sel.inst_div_w    |
                inst_sel.inst_mod_w    |
                inst_sel.inst_div_wu   |
                inst_sel.inst_mod_wu   |
                inst_sel.inst_csrwr    |
                inst_sel.inst_csrxchg  |
                inst_sel.inst_tlbsrch  |
                inst_sel.inst_tlbrd    |
                inst_sel.inst_tlbwr    |
                inst_sel.inst_tlbfill  |
                inst_sel.inst_invtlb   |
                inst_sel.inst_sc_w     |
                inst_sel.inst_st_b     |
                inst_sel.inst_st_h     |
                inst_sel.inst_st_w     |
                inst_sel.inst_beq      |
                inst_sel.inst_bne      |
                inst_sel.inst_blt      |
                inst_sel.inst_bge      |
                inst_sel.inst_bltu     |
                inst_sel.inst_bgeu    

    rk_sel :=   inst_sel.inst_csrwr    |
                inst_sel.inst_csrxchg  |
                inst_sel.inst_ertn     |
                inst_sel.inst_idle     |
                inst_sel.inst_sc_w     |
                inst_sel.inst_st_b     |
                inst_sel.inst_st_h     |
                inst_sel.inst_st_w     |
                inst_sel.inst_beq      |
                inst_sel.inst_bne      |
                inst_sel.inst_blt      |
                inst_sel.inst_bge      |
                inst_sel.inst_bltu     |
                inst_sel.inst_bgeu     

    rk := Mux(rk_valid,Mux(rk_sel, inst(4,0), inst(14,10)), 0.U)

    //rd,rd_valid赋值
    rd_valid := (inst_sel.inst_rdcntid_w| 
                    inst_sel.inst_rdcntvl_w|
                    inst_sel.inst_rdcntvh_w|
                    inst_sel.inst_add_w    |
                    inst_sel.inst_sub_w    |
                    inst_sel.inst_slt      |
                    inst_sel.inst_sltu     |
                    inst_sel.inst_nor      |
                    inst_sel.inst_and      |
                    inst_sel.inst_or       |
                    inst_sel.inst_xor      |
                    inst_sel.inst_sll_w    |
                    inst_sel.inst_srl_w    |
                    inst_sel.inst_sra_w    |
                    inst_sel.inst_mul_w    |
                    inst_sel.inst_mulh_w   |
                    inst_sel.inst_mulh_wu  |
                    inst_sel.inst_div_w    |
                    inst_sel.inst_mod_w    |
                    inst_sel.inst_div_wu   |
                    inst_sel.inst_mod_wu   |
                    inst_sel.inst_slli_w   |
                    inst_sel.inst_srli_w   |
                    inst_sel.inst_srai_w   |
                    inst_sel.inst_slti     |
                    inst_sel.inst_sltui    |
                    inst_sel.inst_addi_w   |
                    inst_sel.inst_andi     |
                    inst_sel.inst_ori      |
                    inst_sel.inst_xori     |
                    inst_sel.inst_csrrd    |
                    inst_sel.inst_csrwr    |
                    inst_sel.inst_csrxchg  |
                    inst_sel.inst_tlbsrch  |
                    inst_sel.inst_tlbrd    |
                    inst_sel.inst_tlbwr    |
                    inst_sel.inst_tlbfill  |
                    inst_sel.inst_lu12i_w  |
                    inst_sel.inst_pcaddu12i|
                    inst_sel.inst_ll_w     |
                    inst_sel.inst_sc_w     |
                    inst_sel.inst_ld_b     |
                    inst_sel.inst_ld_h     |
                    inst_sel.inst_ld_w     |
                    inst_sel.inst_ld_bu    |
                    inst_sel.inst_ld_hu    |
                    inst_sel.inst_jirl     |
                    inst_sel.inst_bl       ) & rd =/= 0.U(5.W)

    rd := Mux(inst_sel.inst_rdcntid_w, inst(9,5), Mux(inst_sel.inst_bl, 1.U(5.W), inst(4,0)))

    //立即数生成器
    val imm_sel = Wire(new imm_sel)
    imm_sel.imm_00U:= ~(inst_sel.inst_slli_w   |
                        inst_sel.inst_srli_w   |
                        inst_sel.inst_srai_w   |
                        inst_sel.inst_andi     |
                        inst_sel.inst_ori      |
                        inst_sel.inst_xori     |
                        inst_sel.inst_slti     |
                        inst_sel.inst_sltui    |
                        inst_sel.inst_addi_w   |
                        inst_sel.inst_ld_b     |
                        inst_sel.inst_ld_h     |
                        inst_sel.inst_ld_w     |
                        inst_sel.inst_st_b     |
                        inst_sel.inst_st_h     |
                        inst_sel.inst_st_w     |
                        inst_sel.inst_ld_bu    |
                        inst_sel.inst_ld_hu    |
                        inst_sel.inst_ll_w     |
                        inst_sel.inst_sc_w     |
                        inst_sel.inst_jirl     |
                        inst_sel.inst_beq      |
                        inst_sel.inst_bne      |
                        inst_sel.inst_blt      |
                        inst_sel.inst_bge      |
                        inst_sel.inst_bltu     |
                        inst_sel.inst_bgeu     |
                        inst_sel.inst_lu12i_w  |
                        inst_sel.inst_pcaddu12i|
                        inst_sel.inst_b        |
                        inst_sel.inst_bl       |
                        inst_sel.inst_csrrd    |
                        inst_sel.inst_csrwr    |
                        inst_sel.inst_csrxchg  |
                        inst_sel.inst_tlbsrch  |
                        inst_sel.inst_rdcntid_w|
                        inst_sel.inst_ertn     |
                        inst_sel.inst_cacop    |
                        inst_sel.inst_invtlb)
    imm_sel.imm_05U :=  inst_sel.inst_slli_w   |
                        inst_sel.inst_srli_w   |
                        inst_sel.inst_srai_w   
    imm_sel.imm_12U :=  inst_sel.inst_andi     |
                        inst_sel.inst_ori      |
                        inst_sel.inst_xori     
    imm_sel.imm_12S :=  inst_sel.inst_slti     |
                        inst_sel.inst_sltui    |
                        inst_sel.inst_addi_w   |
                        inst_sel.inst_ld_b     |
                        inst_sel.inst_ld_h     |
                        inst_sel.inst_ld_w     |
                        inst_sel.inst_st_b     |
                        inst_sel.inst_st_h     |
                        inst_sel.inst_st_w     |
                        inst_sel.inst_ld_bu    |
                        inst_sel.inst_ld_hu    
    imm_sel.imm_14S :=  inst_sel.inst_ll_w     |
                        inst_sel.inst_sc_w     
    imm_sel.imm_16S :=  inst_sel.inst_jirl     |
                        inst_sel.inst_beq      |
                        inst_sel.inst_bne      |
                        inst_sel.inst_blt      |
                        inst_sel.inst_bge      |
                        inst_sel.inst_bltu     |
                        inst_sel.inst_bgeu     
    imm_sel.imm_20S :=  inst_sel.inst_lu12i_w  |
                        inst_sel.inst_pcaddu12i
    imm_sel.imm_26S :=  inst_sel.inst_b        |
                        inst_sel.inst_bl
    imm_sel.imm_CSR :=  inst_sel.inst_csrrd    |
                        inst_sel.inst_csrwr    |
                        inst_sel.inst_csrxchg
    imm_sel.imm_TID :=  inst_sel.inst_tlbsrch  |
                        inst_sel.inst_rdcntid_w
    imm_sel.imm_ERA :=  inst_sel.inst_ertn     
    imm_sel.imm_COP :=  inst_sel.inst_cacop    |
                        inst_sel.inst_invtlb
    val imm_type = imm_sel.asUInt
    imm := Mux1H(Seq(
        imm_type(11) -> 0.U(32.W),
        imm_type(10) -> Cat(0.U(27.W), inst(14, 10)),
        imm_type(9) -> Cat(0.U(20.W), inst(21, 10)),
        imm_type(8) -> Cat(Fill(20, inst(21)), inst(21, 10)),
        imm_type(7) -> Cat(Fill(18, inst(21)), inst(21, 10), 0.U(2.W)),
        imm_type(6) -> Cat(Fill(14, inst(25)), inst(25, 10), 0.U(2.W)),
        imm_type(5) -> Cat(inst(24, 5), 0.U(12.W)),
        imm_type(4) -> Cat(Fill(4, inst(9)), inst(9, 0), inst(25, 10), 0.U(2.W)),
        imm_type(3) -> Cat(0.U(18.W), inst(23, 10)),
        imm_type(2) -> 0x40.U(32.W),
        imm_type(1) -> 0x6.U(32.W),
        imm_type(0) -> Cat(Fill(15, inst(21)), inst(21, 10), inst(4, 0)),
    ))

    //br_type
    val br_valid = Wire(Bool())
    br_valid := inst_sel.inst_beq      |
                inst_sel.inst_bne      |
                inst_sel.inst_blt      |
                inst_sel.inst_bge      |
                inst_sel.inst_bltu     |
                inst_sel.inst_bgeu     |
                inst_sel.inst_b        |
                inst_sel.inst_bl       |
                inst_sel.inst_jirl
    br_type := Mux(br_valid, inst(29,26), 0.U(4.W))

    //mem_type
    val mem_valid = Wire(Bool())
    mem_valid:= inst_sel.inst_ld_b     |
                inst_sel.inst_ld_h     |
                inst_sel.inst_ld_w     |
                inst_sel.inst_st_b     |
                inst_sel.inst_st_h     |
                inst_sel.inst_st_w     |
                inst_sel.inst_ld_bu    |
                inst_sel.inst_ld_hu    |
                inst_sel.inst_ll_w     |
                inst_sel.inst_sc_w
    mem_type := mem_valid ## Mux(mem_valid, inst(25,22), 0.U(4.W))

    //alu_op
    val alu_op_sel = Wire(Vec(11,Bool()))
    alu_op_sel(0) := ~(alu_op_sel(1)|alu_op_sel(2)|alu_op_sel(3)|alu_op_sel(4)|alu_op_sel(5)|alu_op_sel(6)|alu_op_sel(7)|alu_op_sel(8)|alu_op_sel(9)|alu_op_sel(10)) //其它位全零时最低位置1
    alu_op_sel(1) := inst_sel.inst_sub_w |
                     inst_sel.inst_mulh_w
    alu_op_sel(2) := inst_sel.inst_slt   |
                     inst_sel.inst_slti  |
                     inst_sel.inst_mulh_wu
    alu_op_sel(3) := inst_sel.inst_sltu  |
                     inst_sel.inst_sltui
    alu_op_sel(4) := inst_sel.inst_and   |
                     inst_sel.inst_andi  |
                     inst_sel.inst_div_w
    alu_op_sel(5) := inst_sel.inst_nor   |
                     inst_sel.inst_mod_w 
    alu_op_sel(6) := inst_sel.inst_or    |
                     inst_sel.inst_ori   |
                     inst_sel.inst_div_wu
    alu_op_sel(7) := inst_sel.inst_xor   | 
                     inst_sel.inst_xori  | 
                     inst_sel.inst_mod_wu
    alu_op_sel(8) := inst_sel.inst_sll_w | 
                     inst_sel.inst_slli_w
    alu_op_sel(9) := inst_sel.inst_srl_w | 
                     inst_sel.inst_srli_w
    alu_op_sel(10):= inst_sel.inst_sra_w | 
                     inst_sel.inst_srai_w
    alu_op := OHToUInt(alu_op_sel)  

    //ins_type
    ins_type    := (inst_sel.inst_add_w    |
                    inst_sel.inst_sub_w    |
                    inst_sel.inst_slt      |
                    inst_sel.inst_sltu     |
                    inst_sel.inst_nor      |
                    inst_sel.inst_and      |
                    inst_sel.inst_or       |
                    inst_sel.inst_xor      |
                    inst_sel.inst_sll_w    |
                    inst_sel.inst_srl_w    |
                    inst_sel.inst_sra_w    |
                    inst_sel.inst_slli_w   |
                    inst_sel.inst_srli_w   |
                    inst_sel.inst_srai_w   |
                    inst_sel.inst_slti     |
                    inst_sel.inst_sltui    |
                    inst_sel.inst_addi_w   |
                    inst_sel.inst_andi     |
                    inst_sel.inst_ori      |
                    inst_sel.inst_xori     |
                    inst_sel.inst_lu12i_w  |
                    inst_sel.inst_pcaddu12i) ##
                   (inst_sel.inst_cacop    |
                    inst_sel.inst_ll_w     |  
                    inst_sel.inst_sc_w     |  
                    inst_sel.inst_ld_b     |  
                    inst_sel.inst_ld_h     |  
                    inst_sel.inst_ld_w     |  
                    inst_sel.inst_st_b     |  
                    inst_sel.inst_st_h     |  
                    inst_sel.inst_st_w     |  
                    inst_sel.inst_ld_bu    |  
                    inst_sel.inst_ld_hu    |  
                    inst_sel.inst_jirl     |  
                    inst_sel.inst_b        |  
                    inst_sel.inst_bl       |  
                    inst_sel.inst_beq      |  
                    inst_sel.inst_bne      |  
                    inst_sel.inst_blt      |  
                    inst_sel.inst_bge      |  
                    inst_sel.inst_bltu     |  
                    inst_sel.inst_bgeu) ##     
                   (inst_sel.inst_rdcntvl_w|
                    inst_sel.inst_rdcntvh_w|
                    inst_sel.inst_cacop    |
                    inst_sel.inst_ll_w     |
                    inst_sel.inst_sc_w     |
                    inst_sel.inst_ld_b     |
                    inst_sel.inst_ld_h     |
                    inst_sel.inst_ld_w     |
                    inst_sel.inst_st_b     |
                    inst_sel.inst_st_h     |
                    inst_sel.inst_st_w     |
                    inst_sel.inst_ld_bu    |
                    inst_sel.inst_ld_hu)  

    //alu_rs1_sel 
    alu_rs1_sel :=   inst_sel.inst_pcaddu12i|
                    inst_sel.inst_jirl     |
                    inst_sel.inst_bl
    //alu_rs2_sel 
    alu_rs2_sel :=  (inst_sel.inst_rdcntvl_w|
                    inst_sel.inst_rdcntvh_w|
                    inst_sel.inst_jirl     |
                    inst_sel.inst_bl) ##
                   (inst_sel.inst_rdcntvl_w|
                    inst_sel.inst_add_w    |
                    inst_sel.inst_sub_w    |
                    inst_sel.inst_slt      |
                    inst_sel.inst_sltu     |
                    inst_sel.inst_nor      |
                    inst_sel.inst_and      |
                    inst_sel.inst_or       |
                    inst_sel.inst_xor      |
                    inst_sel.inst_sll_w    |
                    inst_sel.inst_srl_w    |
                    inst_sel.inst_sra_w    |
                    inst_sel.inst_mul_w    |
                    inst_sel.inst_mulh_w   |
                    inst_sel.inst_mulh_wu  |
                    inst_sel.inst_div_w    |
                    inst_sel.inst_mod_w    |
                    inst_sel.inst_div_wu   |
                    inst_sel.inst_mod_wu)

   //priv_vec
    priv_vec := {
        val priv_vec = Wire(Vec(13,Bool()))
        priv_vec(1) :=  inst_sel.inst_csrwr
        priv_vec(2) :=  inst_sel.inst_csrxchg
        priv_vec(3) :=  inst_sel.inst_ertn
        priv_vec(4) :=  inst_sel.inst_tlbrd
        priv_vec(5) :=  inst_sel.inst_tlbwr
        priv_vec(6) :=  inst_sel.inst_tlbfill
        priv_vec(7) :=  inst_sel.inst_tlbsrch
        priv_vec(8) :=  inst_sel.inst_invtlb
        priv_vec(9) :=  inst_sel.inst_idle
        priv_vec(10):=  inst_sel.inst_cacop
        priv_vec(11):=  inst_sel.inst_ll_w
        priv_vec(12):=  inst_sel.inst_sc_w
        priv_vec(0) :=  inst_sel.inst_rdcntid_w|
                        inst_sel.inst_csrrd    |
                        VecInit(priv_vec.drop(1)).asUInt.orR
        priv_vec.asUInt
    }

    //exception vector
    val ine_sel = Wire(Bool())
    ine_sel := ~inst_sel.asUInt.orR
    when(ine_sel){
        exception := 0x8d.U(8.W)
    }.elsewhen(inst_sel.inst_break){
        exception := 0xc.U(8.W)
    }.elsewhen(inst_sel.inst_syscall){
        exception := 0x8b.U(8.W)
    }.otherwise{
        exception := 0x00.U(8.W)
    }

    inst_ID.rj := rj
    inst_ID.rk := rk
    inst_ID.rd := rd
    inst_ID.rd_valid := rd_valid
    inst_ID.imm := imm
    inst_ID.alu_op := alu_op
    inst_ID.alu_rs1_sel := alu_rs1_sel
    inst_ID.br_type := br_type
    inst_ID.mem_type := mem_type
    inst_ID.priv_vec := priv_vec
    inst_ID.ins_type := ins_type

    io.inst_ID := inst_ID
}
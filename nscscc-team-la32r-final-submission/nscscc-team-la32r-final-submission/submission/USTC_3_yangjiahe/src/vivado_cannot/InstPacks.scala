import chisel3._
import chisel3.util._
import Configs._
object InstPacks {
    class pack_PF extends Bundle{
        val inst_valid      = Bool()
        val pc              = UInt(32.W)
        val pred_valid      = Bool()
        val pred_jump       = Bool()
        val pred_npc        = UInt(32.W)
        val exception       = UInt(8.W)
        val br_cnt          = UInt(2.W)
    }
    class pack_IF extends pack_PF{
        val inst            = UInt(32.W)
    }
    object pack_IF {
        def apply(a: pack_PF, inst: UInt): pack_IF = {
            val b = Wire(new pack_IF)
            b.inst_valid := a.inst_valid
            b.pc         := a.pc
            b.pred_valid := a.pred_valid
            b.pred_jump  := a.pred_jump
            b.pred_npc   := a.pred_npc
            b.exception  := a.exception
            b.br_cnt     := a.br_cnt
            b.inst       := inst
            b
        }
    }
    class pack_PD extends pack_IF{}
    class pack_ID extends pack_PD{
        val rj              = UInt(5.W)
        val rk              = UInt(5.W)
        val rd              = UInt(5.W)
        val rd_valid        = Bool()
        val imm             = UInt(32.W)
        val alu_op          = UInt(4.W)
        val alu_rs1_sel     = UInt(1.W)
        val alu_rs2_sel     = UInt(2.W)
        val br_type         = UInt(4.W)
        val mem_type        = UInt(5.W)
        val priv_vec        = UInt(13.W)
        val ins_type        = UInt(3.W)
    }
    class pack_RN extends pack_ID{
        val prj             = UInt(PREG_W.W)
        val prk             = UInt(PREG_W.W)
        val prd             = UInt(PREG_W.W)
        val pprd            = UInt(PREG_W.W)
    }
    object pack_RN {
        def apply(a: pack_ID, prj: UInt, prk: UInt, prd: UInt, pprd: UInt): pack_RN = {
            val b = Wire(new pack_RN)
            b.inst_valid := a.inst_valid
            b.pc         := a.pc
            b.pred_valid := a.pred_valid
            b.pred_jump  := a.pred_jump
            b.pred_npc   := a.pred_npc
            b.exception  := a.exception
            b.br_cnt     := a.br_cnt
            b.inst       := a.inst
            b.rj         := a.rj
            b.rk         := a.rk
            b.rd         := a.rd
            b.rd_valid   := a.rd_valid
            b.imm        := a.imm
            b.alu_op     := a.alu_op
            b.alu_rs1_sel:= a.alu_rs1_sel
            b.alu_rs2_sel:= a.alu_rs2_sel
            b.br_type    := a.br_type
            b.mem_type   := a.mem_type
            b.priv_vec   := a.priv_vec
            b.ins_type   := a.ins_type
            b.prj  := prj
            b.prk  := prk
            b.prd  := prd
            b.pprd := pprd
            b
        }
    }
    class pack_DP extends pack_RN{
        val rob_index = UInt(ROB_W.W)
    }
    object pack_DP {
        def apply(a: pack_RN, rob_index: UInt): pack_DP = {
            val b = Wire(new pack_DP)
            b.inst_valid := a.inst_valid
            b.pc         := a.pc
            b.pred_valid := a.pred_valid
            b.pred_jump  := a.pred_jump
            b.pred_npc   := a.pred_npc
            b.exception  := a.exception
            b.br_cnt     := a.br_cnt
            b.inst       := a.inst
            b.rj         := a.rj
            b.rk         := a.rk
            b.rd         := a.rd
            b.rd_valid   := a.rd_valid
            b.imm        := a.imm
            b.alu_op     := a.alu_op
            b.alu_rs1_sel:= a.alu_rs1_sel
            b.alu_rs2_sel:= a.alu_rs2_sel
            b.br_type    := a.br_type
            b.mem_type   := a.mem_type
            b.priv_vec   := a.priv_vec
            b.ins_type   := a.ins_type
            b.prj        := a.prj
            b.prk        := a.prk
            b.prd        := a.prd
            b.pprd       := a.pprd
            b.rob_index  := rob_index
            b
        }
    }
}

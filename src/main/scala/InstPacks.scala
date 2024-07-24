import chisel3._
import chisel3.util._
import Configs._
object InstPacks {
    class pack_PD extends Bundle{
        val inst_valid      = Bool()
        val pc              = UInt(32.W)
        val inst            = UInt(32.W)
        val pred_jump       = Bool()
        val pred_npc        = UInt(32.W)
        val exception       = UInt(8.W)
    }
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
        val fu_id           = UInt(3.W)
    }
    class pack_RN extends pack_ID{
        val prj             = UInt(log2Ceil(PREG_SIZE).W)
        val prk             = UInt(log2Ceil(PREG_SIZE).W)
        val prd             = UInt(log2Ceil(PREG_SIZE).W)
        val pprd            = UInt(log2Ceil(PREG_SIZE).W)
    }
}

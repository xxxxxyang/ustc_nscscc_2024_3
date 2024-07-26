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
    }
    class pack_IF extends pack_PF{
        val inst            = UInt(32.W)
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
}

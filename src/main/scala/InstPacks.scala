import chisel3._
import chisel3.util._
import Configs._
object InstPacks {
    class pack_PD extends Bundle{
        val inst_valid      = Bool()
        val pc              = UInt(32.W)
        val inst            = UInt(32.W)
        val pred_npc        = UInt(32.W)
        val exception       = UInt(8.W)
    }
}

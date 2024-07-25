import chisel3._
import chisel3.util._
object Configs{
    val PREG_SIZE  = 64
    val PREG_W     = log2Ceil(PREG_SIZE)
    val ROB_SIZE   = 32
    val ROB_W      = log2Ceil(ROB_SIZE)
    val FQ_SIZE    = 8 //每个fetch queue的长度
    val FQ_W       = log2Ceil(FQ_SIZE)
    val PC_RESET   = 0x1C000000.U(32.W)
}
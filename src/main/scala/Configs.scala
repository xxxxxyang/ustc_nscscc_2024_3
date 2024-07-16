import chisel3._
import chisel3.util._
object Configs{
    val PREG_SIZE  = 64
    val PREG_W     = log2Ceil(PREG_SIZE)
    val ROB_SIZE   = 32
    val ROB_W      = log2Ceil(ROB_SIZE)
}
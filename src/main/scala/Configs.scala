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

object Predict_Config {
    val BTB_INDEX_WIDTH = 8
    val BTB_TAG_WIDTH   = 28 - BTB_INDEX_WIDTH
    val BTB_DEPTH       = 1 << BTB_INDEX_WIDTH
    val GHR_WIDTH       = 8
    val PHT_INDEX_WIDTH = 12
    val PHT_DEPTH       = 1 << PHT_INDEX_WIDTH

    val RET       = 1.U(2.W)
    val BL        = 2.U(2.W)
    val ICALL     = 3.U(2.W)
    val ELSE      = 0.U(2.W)
}
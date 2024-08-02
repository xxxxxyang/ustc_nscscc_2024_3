import chisel3._
import chisel3.util._
object Configs{
    val PREG_SIZE  = 54
    val PREG_W     = log2Ceil(PREG_SIZE)
    val ROB_SIZE   = 32
    val ROB_W      = log2Ceil(ROB_SIZE)
    val FQ_SIZE    = 8 //每个fetch queue的长度
    val FQ_W       = log2Ceil(FQ_SIZE)
    val PC_RESET   = 0x1C000000.U(32.W)
    val IQ_SIZE    = Seq(8, 8, 8, 8)
    val SB_NUM     = 4
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
object exception_code{
    val INT     = 0x00.U(7.W) // interrupt
    val PIL     = 0x01.U(7.W) // page illegal load
    val PIS     = 0x02.U(7.W) // page illegal store
    val PIF     = 0x03.U(7.W) // page illegal fetch
    val PME     = 0x04.U(7.W) // page maintain exception
    val PPI     = 0x07.U(7.W) // page privilege illegal
    val ADEF    = 0x08.U(7.W) // address exception fetch
    val ADEM    = 0x48.U(7.W) // address exception memory
    val ALE     = 0x09.U(7.W) // address align exception
    val SYS     = 0x0b.U(7.W) // system call
    val BRK     = 0x0c.U(7.W) // breakpoint
    val INE     = 0x0d.U(7.W) // instruction not exist
    val IPE     = 0x0e.U(7.W) // instruction privilege exception
    val FPD     = 0x0f.U(7.W) // floating point disable
    val FPE     = 0x12.U(7.W) // floating point exception
    val TLBR    = 0x3F.U(7.W) // TLB refill
}
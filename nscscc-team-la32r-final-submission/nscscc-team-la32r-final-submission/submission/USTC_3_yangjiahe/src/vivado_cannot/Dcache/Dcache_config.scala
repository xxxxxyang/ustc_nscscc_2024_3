// 32组 两路组相联 32B cache块大小 容量为1KB

import chisel3._
import chisel3.util._

object Dcache_Config{
    val ROB_NUM     = 24
    val INDEX_WIDTH = 5
    val INDEX_DEPTH = 1 << INDEX_WIDTH                  // INDEX_DEPTH = 32

    val OFFSET_WIDTH = 5
    val OFFSET_DEPTH = 1 << OFFSET_WIDTH                // OFFSET_DEPTH = 32

    val TAG_WIDTH = 32 - INDEX_WIDTH - OFFSET_WIDTH     // TAG_WIDTH = 22

    val FROM_CMEM = 0.U(1.W)
    val FROM_RBUF = 1.U(1.W)

    val FROM_PIPE = 0.U(1.W)
    val FROM_SEG  = 1.U(1.W)
}
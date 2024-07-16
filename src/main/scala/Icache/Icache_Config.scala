import chisel3._
import chisel3.util._

// Icache
object Icache_Config{
    val INDEX_WIDTH = 7                                 // 128组 两路组相联 64B cache块大小 容量为8KB
    val INDEX_DEPTH = 1 << INDEX_WIDTH                  // INDEX_DEPTH = 128

    val OFFSET_WIDTH = 6                                // 64B cache块大小 32条指令
    val OFFSET_DEPTH = 1 << OFFSET_WIDTH                // OFFSET_DEPTH = 64

    val TAG_WIDTH = 32 - INDEX_WIDTH - OFFSET_WIDTH     // TAG_WIDTH = 19

    val FROM_CMEM = 0.U(1.W)                            // 从Cache MEM获取指令
    val FROM_RBUF = 1.U(1.W)                            // 从指令缓冲区获取指令

    val FROM_PIPE = 0.U(1.W)                            // 流水线选择信号
    val FROM_SEG  = 1.U(1.W)                            // 段选择信号
}
import chisel3._
import chisel3.util._
import Configs._
object Interfaces {
    /**
     * Dispatch将指令相关信息输到ROB中
     * 返回rob_index
     */
    class DP_to_ROB() extends Bundle{
        val exception   = Output(UInt(8.W))              // 前端产生的异常
        val typ         = Output(UInt(2.W))              // 类型 (store, br, .. )
        val areg        = Output(UInt(5.W))              // 目的逻辑寄存器 (rd)
        val areg_v      = Output(Bool())                 // 是否写
        val preg        = Output(UInt(PREG_W.W))       // 物理寄存器
        val opreg       = Output(UInt(PREG_W.W))       // 原物理寄存器
        val pc          = Output(UInt(32.W))             
        val rob_index   = Input(UInt(ROB_W.W))
    }
    /**
     * 在 WB 阶段，
     * 给定rob_index，通知完成，汇报异常。
     * (写寄存器和分支失败不进ROB)
     */
    class WB_to_ROB() extends Bundle{
        val v           = Bool()                 // 有效
        val rob_index   = UInt(ROB_W.W)
        val exception   = UInt(8.W)              // 后端产生的异常
    }

    class ROB_to_ARAT() extends Bundle{
        val commit_en   = Bool()
        val areg_v      = Bool()
        val preg        = UInt(PREG_W.W)
        val opreg       = UInt(PREG_W.W)
    }
}

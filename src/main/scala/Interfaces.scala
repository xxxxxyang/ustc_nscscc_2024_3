import chisel3._
import chisel3.util._
import Configs._
object Interfaces {
    /**
     * Dispatch将指令相关信息输到ROB中
     * 返回rob_index
     */
    class DP_to_ROB() extends Bundle{
        val exception   = UInt(8.W)              // 前端产生的异常
        val rd          = UInt(5.W)              // 目的逻辑寄存器 (rd)
        val rd_valid    = Bool()                 // 是否写
        val prd         = UInt(PREG_W.W)         // 物理寄存器
        val pprd        = UInt(PREG_W.W)         // 原物理寄存器
        val pc          = UInt(32.W)
        val is_store    = Bool()
        val is_br       = Bool()
        val br_type     = UInt(2.W)              // 用于分支预测
        val priv_vec    = UInt(13.W)
        val inst        = UInt(32.W)             // debug
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
        val rd_valid    = Bool()
        val prd         = UInt(PREG_W.W)
        val pprd        = UInt(PREG_W.W)
    }
}

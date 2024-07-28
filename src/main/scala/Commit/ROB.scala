// package ROB

import chisel3._
import chisel3.util._
import java.{util => ju}
import Configs._
import Interfaces._
import Util.wrap
// TODO: Branch相关
// TODO: 特权

// ROB表项 见P323
class ROB_Item() extends Bundle{
    val exception   = UInt(8.W)
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
    val complete    = Bool()
}

class ROB() extends Module{
    val io = IO(new Bundle{
        //约定：发不出2条指令就全别发，dp_valid = 0
        val dp_valid        = Input(Bool())                    // dp 有效
        val dp              = Input(Vec(2, new DP_to_ROB))
        val rob_index       = Output(Vec(2, UInt(ROB_W.W)))
        val wb              = Input(Vec(4, new WB_to_ROB))
        val arat            = Output(Vec(2, new ROB_to_ARAT))
        val predict_fail    = Input(Bool())
        val full            = Output(Bool())
        val stall           = Input(Bool())
    })

    //rob表 2*(ROB_SIZE/2)
    val rob = RegInit(
        VecInit.fill(2)(
            VecInit.fill(ROB_SIZE / 2)(
                0.U.asTypeOf(new ROB_Item))))
    val head = RegInit(0.U(ROB_W.W)) //最旧
    val tail = RegInit(0.U((ROB_W - 1).W)) //最新
    //tail必定每次进来一行(2个)，head不一定
    
    // dp: 接收dispatch的输入，填入新表项，tail++，返回rob_index
    for(i <- 0 until 2) {
        io.rob_index(i) := 0.U; // default
        when(io.dp_valid) {
            rob(i)(tail).exception := io.dp(i).exception
            rob(i)(tail).rd        := io.dp(i).rd
            rob(i)(tail).rd_valid  := io.dp(i).rd_valid
            rob(i)(tail).prd       := io.dp(i).prd
            rob(i)(tail).pprd      := io.dp(i).pprd
            rob(i)(tail).pc        := io.dp(i).pc
            rob(i)(tail).is_store  := io.dp(i).is_store
            rob(i)(tail).is_br     := io.dp(i).is_br
            rob(i)(tail).br_type   := io.dp(i).br_type
            rob(i)(tail).priv_vec  := io.dp(i).priv_vec
            rob(i)(tail).inst      := io.dp(i).inst
            rob(i)(tail).complete  := false.B
            io.rob_index(i) := Cat(tail, i.U)
            tail := tail + 1.U
        }
    }
    io.full := (tail + 1.U === head)

    // wb: 接收4个FU的结果，填入表项
    for(i <- 0 until 4) {
        when(io.wb(i).v) {
            val col = io.wb(i).rob_index(0)
            val row = io.wb(i).rob_index(ROB_W - 1, 1)
            rob(col)(row).exception := io.wb(i).exception
            rob(col)(row).complete  := true.B
        }
    }
    
    // commit: 将完成的指令退休
    // TODO: empty处理 
    val commit_en  = Wire(Vec(2, Bool())) //P326
    val col = Wire(Vec(2, Bool()))
    val row = Wire(Vec(2, UInt((ROB_W - 1).W)))
    col(0) := head(0)
    col(1) := head(0) ^ 1.U
    row(0) := head(ROB_W - 1, 1)
    row(1) := wrap(head + 1.U, ROB_SIZE.U)(ROB_W - 1, 1) //NOTE: ROB_SIZE为2的幂则不需要wrap
    commit_en(0) := rob(col(0))(row(0)).complete
    commit_en(1) := commit_en(0) && rob(col(1))(row(1)).complete
    val commit_num = PopCount(commit_en)

    //将管理寄存器的操作交给ARAT
    for(i <- 0 until 2) {
        io.arat(i).commit_en := commit_en(i)
        io.arat(i).rd_valid  := rob(col(i))(row(i)).rd_valid
        io.arat(i).prd       := rob(col(i))(row(i)).prd
        io.arat(i).pprd      := rob(col(i))(row(i)).pprd
    }
    head := wrap(head +& PopCount(commit_en), ROB_SIZE.U)
    
} 
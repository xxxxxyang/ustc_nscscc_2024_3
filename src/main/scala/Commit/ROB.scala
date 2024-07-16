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
    val typ         = UInt(2.W)              // 类型 (store, br, .. ) 
    val areg        = UInt(5.W)              // 目的逻辑寄存器 (rd)
    val areg_v      = Bool()                 // 是否写
    val preg        = UInt(PREG_W.W)       // 物理寄存器
    val opreg       = UInt(PREG_W.W)       // 原物理寄存器
    val pc          = UInt(32.W)          
    val complete    = Bool()
}

class ROB() extends Module{
    val io = IO(new Bundle{
        //约定：发不出2条指令就全别发，dp_valid = 0
        val dp_valid = Input(Bool())                    // dp 有效
        val dp = Vec(2, Flipped(new DP_to_ROB))
        val wb = Vec(4, Input(new WB_to_ROB))
        val predict_fail = Input(Bool())
        val arat = Vec(2, Output(new ROB_to_ARAT))
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
    // TODO: full处理
    for(i <- 0 until 2) {
        io.dp(i).rob_index := 0.U; // default
        when(io.dp_valid) {
            rob(i)(tail).exception := io.dp(i).exception
            rob(i)(tail).typ       := io.dp(i).typ
            rob(i)(tail).areg      := io.dp(i).areg
            rob(i)(tail).areg_v    := io.dp(i).areg_v
            rob(i)(tail).preg      := io.dp(i).preg
            rob(i)(tail).opreg     := io.dp(i).opreg
            rob(i)(tail).pc        := io.dp(i).pc
            rob(i)(tail).complete  := false.B
            io.dp(i).rob_index := Cat(tail, i.U)
            tail := tail + 1.U
        }
    }

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
        io.arat(i).areg_v    := rob(col(i))(row(i)).areg_v
        io.arat(i).preg      := rob(col(i))(row(i)).preg
        io.arat(i).opreg     := rob(col(i))(row(i)).opreg
    }
    head := wrap(head +& PopCount(commit_en), ROB_SIZE.U)
    
} 
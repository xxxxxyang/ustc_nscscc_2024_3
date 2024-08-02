import chisel3._
import chisel3.util._
import Configs._
import Interfaces._
import InstPacks._
import Util._

// predict_fail > pd_fix > stall
//(pc, pc+4) 和 inst_valid 将用于取指
class PC() extends Module {
    val io = IO(new Bundle{
        val stall           = Input(Bool())

        val pc              = Output(UInt(32.W))
        val npc             = Output(UInt(32.W))
        val inst_valid      = Output(Vec(2, Bool()))
        val exception       = Output(UInt(8.W))
        // predict
        val pred_jump       = Input(Vec(2, Bool()))
        val pred_npc        = Input(UInt(32.W))
        // predict fail
        val predict_fail    = Input(Bool())
        val branch_target   = Input(UInt(32.W))
        // pd: pre decode
        val pd_fix_en       = Input(Bool())
        val pd_fix_target   = Input(UInt(32.W))
        // idle
        val idle_start      = Input(Bool())
        val idle_intr       = Input(Bool())
    })

    val pc = RegInit(PC_RESET)
    val idle = RegInit(false.B)
    when(io.idle_intr){
        idle := false.B
    }.elsewhen(!idle){
        idle := io.idle_start
    }
    val npc = Wire(UInt(32.W))
    npc := Mux(io.pred_jump.asUInt.orR, io.pred_npc, (pc + 8.U)(31, 3) ## 0.U(3.W))
    when(idle){
        npc := pc
    }.elsewhen(io.predict_fail) {
        npc := io.branch_target
    }.elsewhen(io.pd_fix_en){
        npc := io.pd_fix_target
    }.elsewhen(io.stall){
        npc := pc
    }
    pc := npc

    io.pc  := pc
    io.npc := npc
    
    io.inst_valid(0) := !idle
    io.inst_valid(1) := !idle && !io.pred_jump(0) && !pc(2)

    io.exception  := Mux(pc(1, 0) === 0.U, 0.U, 0x88.U) //ADEF
}
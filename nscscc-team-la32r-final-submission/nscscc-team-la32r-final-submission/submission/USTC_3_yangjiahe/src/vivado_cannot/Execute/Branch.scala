import chisel3._
import chisel3.util._
import control_signal._

class Branch extends Module {
    val io = IO(new Bundle{
        val br_type         = Input(UInt(4.W))
        val src1            = Input(UInt(32.W))
        val src2            = Input(UInt(32.W))
        val pc              = Input(UInt(32.W))
        val imm             = Input(UInt(32.W))
        val pred_jump       = Input(Bool())
        val pred_npc        = Input(UInt(32.W))
        val branch_en       = Output(Bool())
        val branch_target   = Output(UInt(32.W))
        val predict_fail    = Output(Bool())
    })
    io.branch_en     := false.B
    io.branch_target := Mux(io.br_type === BR_JIRL, io.src1, io.pc) + io.imm
    io.predict_fail  := io.branch_en ^ io.pred_jump || (io.pred_jump && io.pred_npc =/= io.branch_target)
    io.branch_en     := MuxLookup(io.br_type, false.B)(Seq(
        BR_BEQ  -> (io.src1 === io.src2),
        BR_BNE  -> (io.src1 =/= io.src2),
        BR_BLT  -> (io.src1.asSInt <  io.src2.asSInt),
        BR_BGE  -> (io.src1.asSInt >= io.src2.asSInt),
        BR_BLTU -> (io.src1 <  io.src2),
        BR_BGEU -> (io.src1 >= io.src2),
        BR_JIRL -> true.B,
        BR_B    -> true.B,
        BR_BL   -> true.B,
    ))
}

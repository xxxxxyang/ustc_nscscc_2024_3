import chisel3._
import chisel3.util._
import control_signal._

class ALU extends Module {
    val io = IO(new Bundle{
        val src1        = Input(UInt(32.W))
        val src2        = Input(UInt(32.W))
        val alu_op      = Input(UInt(4.W))
        val alu_out     = Output(UInt(32.W))
    })

    io.alu_out := MuxLookup(io.alu_op, 0.U)(Seq(
        OP_ADD  -> (io.src1 + io.src2),
        OP_SUB  -> (io.src1 - io.src2),
        OP_SLT  -> (Mux(io.src1.asSInt < io.src2.asSInt, 1.U, 0.U)),
        OP_SLTU -> (Mux(io.src1 < io.src2, 1.U, 0.U)),
        OP_NOR  -> (~(io.src1 | io.src2)),
        OP_AND  -> (io.src1 & io.src2),
        OP_OR   -> (io.src1 | io.src2),
        OP_XOR  -> (io.src1 ^ io.src2),
        OP_SLL  -> (io.src1 << io.src2(4, 0)),
        OP_SRL  -> (io.src1 >> io.src2(4, 0)),
        OP_SRA  -> (io.src1.asSInt >> io.src2(4, 0)).asUInt
    ))
}

import chisel3._
import chisel3.util._

class MDU extends Module {
    val io = IO(new Bundle{
        val src1 = Input(UInt(32.W))
        val src2 = Input(UInt(32.W))
        val mul_res = Output(UInt(32.W))
        val div_res = Output(UInt(32.W))
        val op = Input(UInt(4.W))
        val busy = Output(Bool())
        val inst_valid = Input(Bool())
    })
    val mul = Module(new multiplier)
    val div = Module(new divider)
    mul.io.src1 := io.src1
    mul.io.src2 := io.src2
    mul.io.op := io.op
    mul.io.busy := io.busy

    div.io.inst_valid := io.inst_valid
    div.io.src1 := io.src1
    div.io.src2 := io.src2
    div.io.op := io.op

    io.mul_res := mul.io.res
    io.busy := div.io.busy
    io.div_res := div.io.res

}

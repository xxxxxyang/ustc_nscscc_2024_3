import chisel3._
import chisel3.util._

class Test extends Module{
    val io = IO(new Bundle {
        val x = Input(UInt(32.W))
        val y = Output(UInt(32.W))
    })
    val sub = Module(new Test_sub)
    sub.io.a := true.B
    sub.io.x := io.x
    io.y := sub.io.y
}
class Test_sub extends Module{
    val io = IO(new Bundle {
        val x = Input(UInt(32.W))
        val a = Input(Bool())
        val y = Output(UInt(32.W))
    })
    io.y := ShiftRegister(io.x, 1, io.a)
}
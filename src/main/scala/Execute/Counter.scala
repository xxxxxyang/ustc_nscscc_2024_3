import chisel3._
import chisel3.util._

class CounterModule extends Module{
    val io = IO(new Bundle{
        val cnt = Output(UInt(64.W))
    })
    val cnt = RegInit(0.U(64.W))
    cnt := cnt + 1.U
    io.cnt := cnt
}
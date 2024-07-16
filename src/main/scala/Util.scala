import chisel3._
import chisel3.util._

object Util {
    def wrap(a: UInt, n: UInt): UInt = {
        Mux(a >= n, a - n, a)
    }
}
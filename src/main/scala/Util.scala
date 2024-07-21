import chisel3._
import chisel3.util._

object Util {
    def wrap(a: UInt, n: UInt): UInt = {
        Mux(a >= n, a - n, a)
    }
    def shift1(a: UInt): UInt = {
        val n = a.getWidth
        a(n - 2, 0) ## a(n - 1)
    }
}
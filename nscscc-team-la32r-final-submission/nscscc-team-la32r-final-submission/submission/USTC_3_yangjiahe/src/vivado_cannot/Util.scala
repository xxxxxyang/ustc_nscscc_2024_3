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
    def mask_add1(a: UInt): UInt = { //a<<1|1
        val n = a.getWidth
        a(n - 2, 0) ## 1.U(1.W)
    }
    def mask_add2(a: UInt): UInt = { //a<<2|3
        val n = a.getWidth
        a(n - 3, 0) ## 3.U(2.W)
    }
    def mask_sub1(a: UInt): UInt = { //a>>1
        val n = a.getWidth
        0.U(1.W) ## a(n - 1, 1)
    }
    def reg1[T <: Data](a: T, stall: Bool = false.B, flush: Bool = false.B): T = {
        val reg = RegInit(0.U.asTypeOf(a))
        when(flush){
            reg := 0.U.asTypeOf(a)
        }.elsewhen(!stall){
            reg := a
        }
        reg
    }
    def reg_fw[T <: Data](a: T, forward_en: Bool, forward_data: T, stall: Bool = false.B, flush: Bool = false.B): T = {
        val reg = RegInit(0.U.asTypeOf(a))
        when(flush){
            reg := 0.U.asTypeOf(a)
        }.elsewhen(!stall){
            reg := a
        }.elsewhen(forward_en){
            reg := forward_data
        }
        Mux(forward_en, forward_data, reg)
    }
}
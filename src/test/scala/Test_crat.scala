/*import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.VecLiterals._
import chisel3.experimental.BundleLiterals._

class Test_crat extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "TestModule"
    // test class body here
    it should "test" in {
        test(new CRAT(128)){ c =>
            for(_ <- 0 until 10){
                c.io.rj(0).poke(0.U)
                c.io.rj(1).poke(1.U)
                c.io.rk(0).poke(3.U)
                c.io.rk(1).poke(0.U)
                c.io.rd(0).poke(7.U)
                c.io.rd(1).poke(9.U)
                c.io.rd_valid(0).poke(false.B)
                c.io.rd_valid(1).poke(false.B)
                c.io.alloc_preg(0).poke(10.U)
                c.io.alloc_preg(1).poke(11.U)
                c.io.prj(0).expect(0.U)
                c.clock.step(1)
            }
        }
    }
}
*/
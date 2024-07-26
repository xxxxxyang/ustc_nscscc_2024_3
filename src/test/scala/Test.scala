import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.VecLiterals._
import chisel3.experimental.BundleLiterals._
class TestModule extends Module {
    val io = IO(new Bundle{
        val in  = Input(Vec(2, Bool()))
        val out = Output(UInt(2.W))
    })
    io.out := io.in.asUInt
}
class BasicTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "TestModule"
    // test class body here
    it should "do something" in {
        test(new TestModule){ c =>
            c.io.in.poke(chiselTypeOf(c.io.in).Lit(0 -> true.B, 1 -> false.B))
            c.io.out.expect(1.U)
        }
    }
}
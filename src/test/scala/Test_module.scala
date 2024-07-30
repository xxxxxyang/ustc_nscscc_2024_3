/*import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.VecLiterals._
import chisel3.experimental.BundleLiterals._

class Test_ extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "TestModule"
    // test class body here
    it should "test" in {
        test(new Diff_test_){ c =>
            //测试数据
            val testData = Seq()
            for(){


            }
        }
    }
}

class Diff_test_ extends Module{
    val io = IO(new Bundle{
        val mod1 = new Bundle {}
        val mod2 = new Bundle {}
        val diff = new Bundle {}
    })

    val mod1 = Module(new _)
    val mod2 = Module(new _)

    mod1.io <> io.mod1
    mod2.io <> io.mod2
}*/
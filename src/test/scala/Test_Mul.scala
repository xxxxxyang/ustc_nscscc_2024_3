
import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.VecLiterals._
import chisel3.experimental.BundleLiterals._
import control_signal._

class Test_Mul extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "TestModule"
    // test class body here
    it should "test multipiler" in {
        test(new Diff_test_Mul){ c =>
            //测试数据
            val testData = Seq(
                ("b00111000000000011111100000111001".U, "b10010110001010001111100011011101".U),
                ("b11010110000101000010100011100101".U, "b11010110000110000010000011001110".U),
                ("b10111000100101111011011110010001".U, "b10100011110001000101011111111111".U),
                ("b10000111001001100110011011110011".U, "b11110000011100000001101011010110".U),
                ("b01101000000000001000000000000000".U, "b11100100100100111100000001000010".U),
                ("b11100011100111111110101000000000".U, "b10100000000011101000110011111101".U),
                ("b01011101111100001100000111111111".U, "b10001001011001110100001111011110".U),
                ("b11011110000110010000010011001100".U, "b10101100111110011010101101000001".U),
                ("b01010011111010001001011111100000".U, "b11001100100110101101101110011000".U),
                ("b10101001011101111010100001011100".U, "b00110111001100110001111010101011".U)
            )
            for((in1,in2) <- testData){
                c.clock.step(1)
                c.io.mod1.src1.poke(in1)
                c.io.mod1.src2.poke(in2)
                c.io.mod2.src1.poke(in1)
                c.io.mod2.src2.poke(in2)
                c.io.mod1.op.poke(OP_MUL)
                c.io.mod1.busy.poke(false.B)
                c.io.mod2.op.poke(OP_MUL)
                c.io.mod2.busy.poke(false.B)
                c.clock.step(1)
                c.clock.step(1)
                c.io.out.expect(true.B)
            }
        }
    }
}
class Diff_test_Mul extends Module{
    val io = IO(new Bundle{
        val mod1 = new Bundle{
            val src1 = Input(UInt(32.W))
            val src2 = Input(UInt(32.W))
            val res = Output(UInt(32.W))
            val op = Input(UInt(4.W))
            val busy = Input(Bool())
        }
        val mod2 = new Bundle{
            val src1 = Input(UInt(32.W))
            val src2 = Input(UInt(32.W))
            val op   = Input(UInt(4.W))
            val busy = Input(Bool())
            val res  = Output(UInt(32.W))
        }
        val out = Output(Bool())
    })
    val mod1 = Module(new multiplier)
    val mod2 = Module(new Multiply)
    mod1.io <> io.mod1
    mod2.io <> io.mod2
    io.out := mod1.io.res === mod2.io.res
}

class Multiply extends Module{
    val io = IO(new Bundle{
        val src1 = Input(UInt(32.W))
        val src2 = Input(UInt(32.W))
        val op   = Input(UInt(4.W))
        val busy = Input(Bool())
        val res  = Output(UInt(32.W))
    })

    val src1 = Mux(io.op === OP_MULHU, 0.U(34.W) ## io.src1, Fill(34, io.src1(31)) ## io.src1)
    val src2 = Mux(io.op === OP_MULHU, 0.U(34.W) ## io.src2, Fill(34, io.src2(31)) ## io.src2)

    def Booth2(x: UInt, y: UInt, n: Int) :UInt = {
        val res = WireDefault(x)
        val neg = y(2)
        switch(y){
            is(0.U){ res := 0.U }
            is(3.U){ res := x(n-2, 0) ## 0.U(1.W) }
            is(4.U){ res := x(n-2, 0) ## 0.U(1.W) }
            is(7.U){ res := 0.U }
        }
        Mux(neg, ~res + 1.U, res)
    }
    def CSA(x: UInt, y: UInt, z: UInt, n: Int) : UInt  = {
        val x_xor_y = WireDefault(x ^ y)
        val res1 = WireDefault(x_xor_y ^ z)
        val res2 = WireDefault((((x & y) | z & (x_xor_y)) ## 0.U(1.W))(n-1, 0))
        res2 ## res1
    }

    // stage 1: booth and wallce tree
    // booth encode
    val booth = VecInit.tabulate(33){ i => 
        Booth2(src1(65-2*i, 0) ## 0.U((2*i).W),
            (if(i == 0) src2(1, 0) ## 0.U(1.W) else src2(2*i+1, 2*i-1)), 66
        )
    }
    val booth_reg1 = ShiftRegister(VecInit(booth.take(5)), 1, !io.busy)
    val booth_reg2 = ShiftRegister(VecInit(booth.drop(5).take(5)), 1, !io.busy)
    val booth_reg3 = ShiftRegister(VecInit(booth.drop(10).take(5)), 1, !io.busy)
    val booth_reg4 = ShiftRegister(VecInit(booth.drop(15).take(5)), 1, !io.busy)
    val booth_reg5 = ShiftRegister(VecInit(booth.drop(20).take(5)), 1, !io.busy)
    val booth_reg6 = ShiftRegister(VecInit(booth.drop(25).take(5)), 1, !io.busy)
    val booth_reg7 = ShiftRegister(VecInit(booth.drop(30).take(3)), 1, !io.busy)

    val booth_reg = VecInit.tabulate(33){i => 
        if(i < 5) booth_reg1(i)
        else if(i < 10) booth_reg2(i-5)
        else if(i < 15) booth_reg3(i-10)
        else if(i < 20) booth_reg4(i-15)
        else if(i < 25) booth_reg5(i-20)
        else if(i < 30) booth_reg6(i-25)
        else booth_reg7(i-30)
    }
    // wallce tree
    // level1: input 33, output 22
    val level1         = VecInit.tabulate(11){i => CSA(booth_reg(3*i), booth_reg(3*i+1), booth_reg(3*i+2), 66)}
    val res1           = Wire(Vec(22, UInt(66.W)))
    for(i <- 0 until 11){
        res1(2*i)       := level1(i)(65, 0)
        res1(2*i+1)     := level1(i)(131, 66)
    }
    // level2: input 22, output 15
    val level2         = VecInit.tabulate(7){i => CSA(res1(3*i), res1(3*i+1), res1(3*i+2), 66)}
    val res2           = Wire(Vec(15, UInt(66.W)))
    for(i <- 0 until 7){
        res2(2*i)       := level2(i)(65, 0)
        res2(2*i+1)     := level2(i)(131, 66)
    }
    res2(14) := res1(21)
    // level3: input 15, output 10
    val level3         = VecInit.tabulate(5){i => CSA(res2(3*i), res2(3*i+1), res2(3*i+2), 66)}
    val res3           = Wire(Vec(10, UInt(66.W)))
    for(i <- 0 until 5){
        res3(2*i)       := level3(i)(65, 0)
        res3(2*i+1)     := level3(i)(131, 66)
    }
    // level4: input 10, output 7
    val level4         = VecInit.tabulate(3){i => CSA(res3(3*i), res3(3*i+1), res3(3*i+2), 66)}
    val res4           = Wire(Vec(7, UInt(66.W)))
    for(i <- 0 until 3){
        res4(2*i)       := level4(i)(65, 0)
        res4(2*i+1)     := level4(i)(131, 66)
    }
    res4(6) := res3(9)
    val level5        = VecInit.tabulate(2){i => CSA(res4(3*i), res4(3*i+1), res4(3*i+2), 66)}
    val res5          = Wire(Vec(5, UInt(66.W)))
    for(i <- 0 until 2){
        res5(2*i)       := level5(i)(65, 0)
        res5(2*i+1)     := level5(i)(131, 66)
    }
    res5(4) := res4(6)
    // level6: input 5, output 4
    val level6     = VecInit.tabulate(1){i => CSA(res5(0), res5(1), res5(2), 66)}
    val res6       = Wire(Vec(4, UInt(66.W)))
    res6(0)        := level6(0)(65, 0)
    res6(1)        := level6(0)(131, 66)
    res6(2)        := res5(3)
    res6(3)        := res5(4)
    // level7: input 4, output 3
    val level7      = VecInit.tabulate(1){i => CSA(res6(0), res6(1), res6(2), 66)}
    val res7        = Wire(Vec(3, UInt(66.W)))
    res7(0)         := level7(0)(65, 0)
    res7(1)         := level7(0)(131, 66)
    res7(2)         := res6(3)
    // level8: input 3, output 2
    val level8      = VecInit.tabulate(1){i => CSA(res7(0), res7(1), res7(2), 66)}
    val res8        = Wire(Vec(2, UInt(66.W)))
    res8(0)         := level8(0)(65, 0)
    res8(1)         := level8(0)(131, 66)

    // register
    val adder_src1   = ShiftRegister(res8(0)(63, 0), 1, !io.busy)
    val adder_src2   = ShiftRegister(res8(1)(63, 0), 1, !io.busy)
    val op_reg1      = ShiftRegister(io.op, 1, !io.busy)
    val op_reg2      = ShiftRegister(op_reg1, 1, !io.busy)
    val res         = adder_src1 + adder_src2
    io.res          := Mux(op_reg2 === OP_MUL, res(31, 0), res(63, 32))

}

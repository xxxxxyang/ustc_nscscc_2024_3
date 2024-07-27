import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.VecLiterals._
import chisel3.experimental.BundleLiterals._
import control_signal._


class Test_Div extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "TestModule"
    // test class body here
    it should "test divider" in {
        test(new Diff_test_div){ c =>
            //测试数据
            val testData = Seq(
                //("b00111000000000011111100000111001".U, "b10010110001010001111100011011101".U),
                //("b11010110000101000010100011100101".U, "b11010110000110000010000011001110".U),
                //("b10111000100101111011011110010001".U, "b10100011110001000101011111111111".U),
                //("b10000111001001100110011011110011".U, "b11110000011100000001101011010110".U),
                //("b01101000000000001000000000000000".U, "b11100100100100111100000001000010".U),
                //("b11100011100111111110101000000000".U, "b10100000000011101000110011111101".U),
                //("b01011101111100001100000111111111".U, "b10001001011001110100001111011110".U),
                //("b11011110000110010000010011001100".U, "b10101100111110011010101101000001".U),
                ("b01010011111010001001011111100000".U, "b11001100100110101101101110011000".U),
                //("b10101001011101111010100001011100".U, "b00110111001100110001111010101011".U)
                //("b00000000000000000000000000011110".U, "b00000000000000000000000000000010".U)
            )
            for((in1,in2) <- testData){
                //c.clock.step(1)
                c.io.mod1.src1.poke(in1)
                c.io.mod1.src2.poke(in2)
                c.io.mod2.src1.poke(in1)
                c.io.mod2.src2.poke(in2)
                c.io.mod1.op.poke(OP_DIVU)
                c.io.mod2.op.poke(OP_DIVU)
                c.io.mod1.inst_valid.poke(true.B)
                c.io.set.poke(false.B)
                c.clock.step(2)
                c.io.set.poke(true.B)
                c.clock.step(1)
                c.io.set.poke(false.B)
                c.clock.step(50)
                c.io.out.expect(true.B)
                //c.io.mod1.res.expect(3.U)
                //c.io.out_res2.expect(3.U)
            }
        }
    }
}

class Diff_test_div extends Module{
    val io = IO(new Bundle{
        val mod1 = new Bundle{
            val src1 = Input(UInt(32.W))
            val src2 = Input(UInt(32.W))
            val res = Output(UInt(32.W))
            val op = Input(UInt(4.W))
            val busy = Output(Bool())
            val inst_valid = Input(Bool())
        }
        val mod2 = new Bundle{
            val src1 = Input(UInt(32.W))
            val src2 = Input(UInt(32.W))
            val op   = Input(UInt(4.W))
            val busy = Output(Bool())
            val res  = Output(UInt(32.W))
        }
        val out = Output(Bool())
        val out_res1 = Output(UInt(32.W))
        val out_res2 = Output(UInt(32.W))
        val set = Input(Bool())
    })
    val mod1 = Module(new divider)
    val mod2 = Module(new Divide)
    mod1.io <> io.mod1
    mod2.io <> io.mod2
    val res_1 = RegInit(0.U(32.W))
    val res_2 = RegInit(0.U(32.W))
    val busy_1 = ShiftRegister(mod1.io.busy, 1, true.B)
    val busy_2 = ShiftRegister(mod2.io.busy, 1, true.B)
    when(~mod1.io.busy && busy_1){
        res_1 := mod1.io.res
    }.elsewhen(io.set){
        res_1 := mod1.io.res
    }
    when(~mod2.io.busy && busy_2){
        res_2 := mod2.io.res
    }
    io.out := res_1 === res_2
    io.out_res1 := res_1
    io.out_res2 := res_2
}

class Divide extends Module{
    val io = IO(new Bundle{
        val src1        = Input(UInt(32.W))
        val src2        = Input(UInt(32.W))
        val op          = Input(UInt(4.W))
        val res         = Output(UInt(32.W))
        val busy        = Output(Bool())
    })

    val en = io.op(2)
    // stage1 : record src2 and op and sign
    val res_sign = MuxLookup(io.op, false.B)(Seq(
        OP_DIV  -> (io.src1(31) ^ io.src2(31)),
        OP_DIVU -> false.B,
        OP_MOD  -> io.src1(31),
        OP_MODU -> false.B
    ))
    val src1 = Mux((io.op === OP_DIV || io.op === OP_MOD) && io.src1(31), ~io.src1 + 1.U, io.src1)
    val src2 = Mux((io.op === OP_DIV || io.op === OP_MOD) && io.src2(31), ~io.src2 + 1.U, io.src2)

    // get highest 1 in src1
    val high_rev = PriorityEncoder(Reverse(src1))
    val cnt = RegInit(VecInit.fill(32)(0.U(6.W)))

    val src1_reg1 = ShiftRegister(src1, 1, !io.busy)
    val src2_reg1 = ShiftRegister(src2, 1, !io.busy)
    val op_reg1 = ShiftRegister(io.op, 1, !io.busy)
    val res_sign_reg1 = ShiftRegister(res_sign, 1, !io.busy)
    val en_reg1 = ShiftRegister(en, 1, !io.busy)
    val high_rev_reg1 = ShiftRegister(high_rev, 1, !io.busy)

    val src2_reg2 = RegInit(0.U(32.W))
    val op_reg2 = RegInit(0.U(5.W))
    val res_sign_reg2 = RegInit(false.B)

    // when(cnt(4) =/= 0.U){
        
    // }.elsewhen(en_reg1){
    //     cnt := 33.U - high_rev_reg1
    // }
    for(i <- 0 until 32){
        when(cnt(i) =/= 0.U){
            cnt(i) := cnt(i) - 1.U
        }.elsewhen(en_reg1){
            cnt(i) := 33.U - high_rev_reg1
        }
    }

    when(en_reg1 && cnt(4) === 0.U){
        src2_reg2 := src2_reg1
        op_reg2 := op_reg1
        res_sign_reg2 := res_sign_reg1
    }

    val quo_rem = RegInit(0.U(65.W))
    when(cnt(5) =/= 0.U){
        when(quo_rem(63, 32) >= src2_reg2){
            quo_rem := (quo_rem(63, 32) - src2_reg2) ## quo_rem(31, 0) ## 1.U(1.W)
        }.otherwise{
            quo_rem := (quo_rem(63, 0) ## 0.U(1.W))
        }
    }.elsewhen(en_reg1){
        quo_rem := (0.U(33.W) ## src1_reg1) << high_rev_reg1
    }

    // io.busy := cnt =/= 0.U
    io.busy := cnt(0) =/= 0.U
    
    io.res := MuxLookup(op_reg2, 0.U(32.W))(Seq(
        OP_DIV  -> Mux(res_sign_reg2, ~quo_rem(31, 0) + 1.U, quo_rem(31, 0)),
        OP_DIVU -> quo_rem(31, 0),
        OP_MOD  -> Mux(res_sign_reg2, ~quo_rem(64, 33) + 1.U, quo_rem(64, 33)),
        OP_MODU -> quo_rem(64, 33)
    ))
}

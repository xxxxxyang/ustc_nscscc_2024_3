import chisel3._
import chisel3.util._

class multiplier extends Module{
    val io = IO(new Bundle{
        val src1 = Input(UInt(32.W))
        val src2 = Input(UInt(32.W))
        val res = Output(UInt(32.W))
        val op = Input(UInt(4.W))
        val busy = Input(Bool())
    })

    //stage 1 —— Booth coding
    def Booth(x:UInt,y:UInt,n:Int):UInt = {
        val res = WireDefault(x)
        switch(y){
            is(0.U){ res := 0.U}
            is(1.U){ res := x}
            is(2.U){ res := x}
            is(3.U){ res := x(n-2,0) ## 0.U(1.W)}
            is(4.U){ res := ~(x(n-2,0) ## 0.U(1.W)) + 1.U}
            is(5.U){ res := ~x + 1.U}
            is(6.U){ res := ~x + 1.U}
            is(7.U){ res := 0.U}
        }
        res
    }
    val src1 = Wire(UInt(66.W))
    src1 := Cat(Mux(io.op(1), 0.U(34.W), Fill(34,io.src1(31))),io.src1)
    val src2 = Wire(UInt(66.W))
    src2 := Cat(Mux(io.op(1), 0.U(34.W), Fill(34,io.src2(31))),io.src2)
    val booth_res = Wire(VecInit.tabulate(17)(i => Booth(src1(65-2*i,0) ## 0.U((2*i).W),(if(i==0) src2(1, 0) ## 0.U(1.W) else src2(2*i+1, 2*i-1)), 66)))
    val booth_reg = RegInit(VecInit.fill(17)(0.U(66.W)))
    when(!io.busy){
        booth_reg := booth_res
    }
    val op_stage1 = RegInit(0.U(4.W))
    when(!io.busy){
        op_stage1 := io.op
    }
    //stage 2 —— Wallet Tree
    def CSA(x:UInt,y:UInt,z:UInt,n:Int):UInt = {
        val res1 = WireDefault((x^y)^z)
        val res2 = WireDefault(((((x^y)&z)|(x&y)) ## 0.U(1.W))(n-1,0))
        res2 ## res1
    }
    //input:17 output:12
    //input:12 output:8
    //input:8 output:6
    //input:6 output:4
    //input:4 output:3
    //input:3 output:2
    //stage 3 —— ADD
}
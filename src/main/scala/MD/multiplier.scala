import chisel3._
import chisel3.util._
import control_signal._

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
    val booth_res = VecInit.tabulate(17)(i => Booth(src1(65-2*i,0) ## 0.U((2*i).W),(if(i==0) src2(1, 0) ## 0.U(1.W) else src2(2*i+1, 2*i-1)), 66))
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
    val temp1 = Wire(Vec(12,UInt(66.W)))
    for(i <- 0 until 5){
        val t1 = CSA(booth_reg(3*i),booth_reg(3*i+1),booth_reg(3*i+2),66)
        temp1(2*i) := t1(65,0)
        temp1(2*i+1) := t1(131,66)
    }
    temp1(10) := booth_reg(15)
    temp1(11) := booth_reg(16)
    //input:12 output:8
    val temp2 = Wire(Vec(8,UInt(66.W)))
    for(i <- 0 until 4){
        val t2 = Wire(UInt(132.W))
        t2 := CSA(temp1(3*i),temp1(3*i+1),temp1(3*i+2),66)
        temp2(2*i) := t2(65,0)
        temp2(2*i+1) := t2(131,66)
    }
    //input:8 output:6
    val temp3 = Wire(Vec(6,UInt(66.W)))
    for(i <- 0 until 2){
        val t3 = Wire(UInt(132.W))
        t3 := CSA(temp2(3*i),temp2(3*i+1),temp2(3*i+2),66)
        temp3(2*i) := t3(65,0)
        temp3(2*i+1) := t3(131,66)
    }
    temp3(4) := temp2(6)
    temp3(5) := temp2(7)
    //input:6 output:4
    val temp4 = Wire(Vec(4,UInt(66.W)))
    for(i <- 0 until 2){
        val t4 = Wire(UInt(132.W))
        t4 := CSA(temp3(3*i),temp3(3*i+1),temp3(3*i+2),66)
        temp4(2*i) := t4(65,0)
        temp4(2*i+1) := t4(131,66)
    }
    //input:4 output:3
    val temp5 = Wire(Vec(3,UInt(66.W)))
    val t5 = Wire(UInt(132.W))
    t5 := CSA(temp4(0),temp4(1),temp4(2),66)
    temp5(0) := t5(65,0)
    temp5(1) := t5(131,66)
    temp5(2) := temp4(3)
    //input:3 output:2
    val temp6 = Wire(Vec(2,UInt(66.W)))
    val t6 = Wire(UInt(132.W))
    t6 := CSA(temp5(0),temp5(1),temp5(2),66)
    temp6(0) := t6(65,0)
    temp6(1) := t6(131,66)

    //register of the wallet tree's result
    val tree_temp = RegInit(VecInit.fill(2)(0.U(66.W)))
    when(!io.busy){
        tree_temp := temp6
    }
    val op_stage2 = RegInit(0.U(4.W))
    when(!io.busy){
        op_stage2 := op_stage1
    }

    //stage 3 —— ADD
    val res_temp = Wire(UInt(66.W))
    res_temp := tree_temp(0) + tree_temp(1)
    io.res := Mux((op_stage2 === OP_MUL),res_temp(31,0),res_temp(63,32))
}
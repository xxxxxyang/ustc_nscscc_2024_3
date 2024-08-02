import chisel3._
import chisel3.util._
import control_signal._

class divider extends Module{
    val io = IO(new Bundle{
        val src1 = Input(UInt(32.W))
        val src2 = Input(UInt(32.W))
        val op = Input(UInt(4.W))
        val res = Output(UInt(32.W))
        val inst_valid = Input(Bool())
        val busy = Output(Bool())
    })
    val cnt = RegInit(0.U(6.W))
    //stage 1 —— preparation
    //inst_valid register
    val en = RegInit(false.B)
    when(!io.busy){
        en := io.op(2) & io.inst_valid
    }
    //src1 absolute value
    val ab_scr1 = RegInit(0.U(32.W))
    when(!io.busy){
        ab_scr1 := Mux(((io.op === OP_MOD)||(io.op === OP_DIV))&&io.src1(31), ~io.src1+1.U , io.src1)
    }
    //src2 absolute value
    val ab_scr2_stage1 = RegInit(0.U(32.W))
    when(!io.busy){
        ab_scr2_stage1 := Mux(((io.op === OP_MOD)||(io.op === OP_DIV))&&io.src2(31), ~io.src2+1.U , io.src2)
    }
    //result sign
    val res_sign_stage1 = RegInit(false.B)
    when(!io.busy){
        res_sign_stage1 := Mux(io.op(1),false.B,Mux(io.op(0),io.src1(31),io.src1(31)^io.src2(31)))
    }
    //operation code
    val op_stage1 = RegInit(0.U(4.W))
    when(!io.busy){
        op_stage1 := io.op
    }
    //highest 1 in src1
    val H1_src1 = RegInit(0.U(5.W))
    when(!io.busy){
        H1_src1 := PriorityEncoder(Reverse(Mux(((io.op === OP_MOD)||(io.op === OP_DIV))&&io.src1(31), ~io.src1+1.U , io.src1)))
    }
    //highest 1 in src2
    val H1_src2 = RegInit(0.U(5.W))
    when(!io.busy){
        H1_src2 := PriorityEncoder(Reverse(Mux(((io.op === OP_MOD)||(io.op === OP_DIV))&&io.src2(31), ~io.src2+1.U , io.src2)))
    }
    //offset
    val offset = Wire(UInt(6.W))
    offset := Mux((H1_src1 <= H1_src2), (H1_src1 + 32.U) - H1_src2, 33.U)
    //cnt_init
    val cnt_init = Wire(UInt(6.W))
    cnt_init := Mux((H1_src1 <= H1_src2), (H1_src2 + 1.U) - H1_src1, 0.U)

    //stage 2 —— calculation
    //src2 absolute value
    val ab_scr2_stage2 = RegInit(0.U(32.W))
    when(!io.busy){
        ab_scr2_stage2 := ab_scr2_stage1
    }
    //result sign
    val res_sign_stage2 = RegInit(false.B)
    when(!io.busy){
        res_sign_stage2 := res_sign_stage1
    }
    //operation code
    val op_stage2 = RegInit(0.U(4.W))
    when(!io.busy){
        op_stage2 := op_stage1
    }
    //quotient
    val quo = RegInit(0.U(65.W))
    val sel = Wire(Bool())
    sel := quo(63,32) >= ab_scr2_stage2
    when(cnt =/= 0.U){
        quo := Mux(sel,quo(63,32)-ab_scr2_stage2,quo(63,32)) ## quo(31,0) ## sel
    }.elsewhen(en){
        quo := (0.U(33.W) ## ab_scr1) << offset
    }
    //counter
    when(cnt =/= 0.U){
        cnt := cnt - 1.U
    }.elsewhen(en){
        cnt := cnt_init
    }
    //busy
    io.busy := cnt =/= 0.U

    //res
    val res_temp = Mux(op_stage2(0),quo(64,33),quo(31,0))
    io.res := Mux((~op_stage2(1))&&res_sign_stage2,~res_temp+1.U,res_temp)
}
import chisel3._
import chisel3.util._

class Rename(n:Int) extends Module {  
    val io = IO(new Bundle{
        val rj = Input(Vec(2,UInt(5.W)))            //指令的rj寄存器编号
        val rk = Input(Vec(2,UInt(5.W)))            //指令的rk寄存器编号

        val rd = Input(Vec(2,UInt(5.W)))            //指令的rd寄存器编号
        val rd_valid = Input(Vec(2,Bool()))         //指令是否有rd寄存器（是否有写操作）
        val alloc_preg = Input(Vec(2,UInt(log2Ceil(n).W)))    //从Freelist中分配的物理寄存器编号

        val prj = Output(Vec(2,UInt(log2Ceil(n).W)))          //RAT中rj寄存器的物理寄存器编号
        val prk = Output(Vec(2,UInt(log2Ceil(n).W)))        //RAT中rk寄存器的物理寄存器编号
        val prd = Output(Vec(2,UInt(log2Ceil(n).W)))          //RAT中rd寄存器的物理寄存器编号
        val pprd = Output(Vec(2,UInt(log2Ceil(n).W)))         //RAT中rd寄存器的旧的物理寄存器编号

        val prj_ready = Output(Vec(2,Bool()))       //rj物理寄存器的ready信号
        val prk_ready = Output(Vec(2,Bool()))       //rk物理寄存器的ready信号

        val predict_fail = Input(Bool())             //分支预测错误信号
        val arch_rat_valid = Input(Vec(32,Bool()))   //ROB维护的architectural RAT中valid位的信息

        val rename_en = Input(Vec(2,Bool()))                //重命名使能信号,若开启则会使CRAT锁定

        val wake_preg = Input(Vec(4,UInt(log2Ceil(n).W)))  //唤醒的物理寄存器编号
        val wake_valid = Input(Vec(4,Bool()))              //唤醒的物理寄存器是否有效
    })

    //CRAT表
    val crat = Module(new CRAT(n))

    //rd的物理寄存器编号
    crat.io.rd := io.rd
    io.prd := io.alloc_preg

    //处理WAW相关
    crat.io.rd_valid(1) := io.rd_valid(1)&io.rename_en(1)
    crat.io.rd_valid(0) := Mux(io.rd_valid(1)&(io.rd(0)===io.rd(1)),false.B,io.rd_valid(0)&io.rename_en(0)) 
    io.pprd(1) := crat.io.pprd(1)
    io.pprd(0) := Mux(io.rd_valid(1)&(io.rd(0)===io.rd(1)),io.prd(1),crat.io.pprd(0))

    //rj和rk的物理寄存器编号,处理RAW相关
    crat.io.rj := io.rj
    crat.io.rk := io.rk
    io.prj(0) := crat.io.prj(0)
    io.prj(1) := Mux(io.rd_valid(0)&(io.rd(0)===io.rj(1)),io.prd(0),crat.io.prj(1))
    io.prk(0) := crat.io.prk(0)
    io.prk(1) := Mux(io.rd_valid(0)&(io.rd(0)===io.rk(1)),io.prd(0),crat.io.prk(1))

    //给CRAT剩余部分接线
    crat.io.alloc_preg := io.alloc_preg
    crat.io.predict_fail := io.predict_fail
    crat.io.arch_rat_valid := io.arch_rat_valid
    crat.io.wake_preg := io.wake_preg
    crat.io.wake_valid := io.wake_valid
    crat.io.prj_ready := io.prj_ready
    crat.io.prk_ready := io.prk_ready
}
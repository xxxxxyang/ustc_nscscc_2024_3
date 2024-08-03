import chisel3._
import chisel3.util._
import Configs._

class Bypass() extends Module {
    val io = IO(new Bundle{
        //wb: ALU1, ALU2, LS
        val rd_valid_wb      = Input(Vec(3, Bool()))
        val prd_wb           = Input(Vec(3, UInt(PREG_W.W)))
        val prd_wdata_wb     = Input(Vec(3, UInt(32.W)))
        //ex
        val prj_ex           = Input(Vec(4, UInt(PREG_W.W)))
        val prk_ex           = Input(Vec(4, UInt(PREG_W.W)))
        val forward_prj_en   = Output(Vec(4, Bool()))
        val forward_prk_en   = Output(Vec(4, Bool()))
        val forward_prj_data = Output(Vec(4, UInt(32.W)))
        val forward_prk_data = Output(Vec(4, UInt(32.W)))
    })

    for(i <- 0 until 3){
        val hit_j = VecInit.tabulate(2)(j => (io.rd_valid_wb(j) && io.prd_wb(j) === io.prj_ex(i) && io.prd_wb(j).orR))
        io.forward_prj_en(i) :=  hit_j.asUInt.orR
        io.forward_prj_data(i) := Mux(io.prd_wb(0) === io.prj_ex(i), io.prd_wdata_wb(0), io.prd_wdata_wb(1))

        val hit_k = VecInit.tabulate(2)(j => (io.rd_valid_wb(j) && io.prd_wb(j) === io.prk_ex(i) && io.prd_wb(j).orR))
        io.forward_prk_en(i) :=  hit_k.asUInt.orR
        io.forward_prk_data(i) := Mux(io.prd_wb(0) === io.prk_ex(i), io.prd_wdata_wb(0), io.prd_wdata_wb(1))
    }
    
    //LS
    val hit_j = VecInit.tabulate(3)(j => (io.rd_valid_wb(j) && io.prd_wb(j) === io.prj_ex(3) && io.prd_wb(j).orR))
    io.forward_prj_en(3) :=  hit_j.asUInt.orR
    io.forward_prj_data(3) := Mux1H(hit_j, io.prd_wdata_wb)

    val hit_k = VecInit.tabulate(3)(j => (io.rd_valid_wb(j) && io.prd_wb(j) === io.prk_ex(3) && io.prd_wb(j).orR))
    io.forward_prk_en(3) :=  hit_k.asUInt.orR
    io.forward_prk_data(3) := Mux1H(hit_j, io.prd_wdata_wb)
}
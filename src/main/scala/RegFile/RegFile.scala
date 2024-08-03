import chisel3._
import chisel3.util._
import Configs._
import Interfaces._
import InstPacks._
import Util._

class RegFile extends Module{
    val io = IO(new Bundle{
        val prj       = Input(Vec(4, UInt(PREG_W.W)))
        val prk       = Input(Vec(4, UInt(PREG_W.W)))
        val prj_data  = Output(Vec(4, UInt(32.W)))
        val prk_data  = Output(Vec(4, UInt(32.W)))
    
        val prd       = Input(Vec(4, UInt(PREG_W.W)))
        val wdata     = Input(Vec(4, UInt(32.W)))
        val we        = Input(Vec(4, Bool()))
    })
    val rf = RegInit(VecInit.fill(PREG_SIZE)(0.U(32.W)))
    val we = VecInit.tabulate(4)(i => io.we(i) && io.prd(i).orR)
    for(i <- 0 until 4){
        when(we(i)){
            rf(io.prd(i)) := io.wdata(i)
        }
    }
    for(i <- 0 until 4){
        // write first
        {
            val w_hit = VecInit.tabulate(4)(j => we(j) && io.prd(j) === io.prj(i)).asUInt
            io.prj_data(i) := Mux(w_hit.orR, Mux1H(w_hit, io.wdata), rf(io.prj(i)))
        }
        {
            val w_hit = VecInit.tabulate(4)(j => we(j) && io.prd(j) === io.prk(i)).asUInt
            io.prk_data(i) := Mux(w_hit.orR, Mux1H(w_hit, io.wdata), rf(io.prk(i)))
        }
    }
}
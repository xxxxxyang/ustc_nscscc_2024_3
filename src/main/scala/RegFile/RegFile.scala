import chisel3._
import chisel3.util._
import Configs._
import Interfaces._
import InstPacks._
import Util._

class RegFile(n: Int) extends Module{
    val io = IO(new Bundle{
        val prj       = Input(Vec(4, UInt(log2Ceil(n).W)))
        val prk       = Input(Vec(4, UInt(log2Ceil(n).W)))
        val prj_data  = Output(Vec(4, UInt(32.W)))
        val prk_data  = Output(Vec(4, UInt(32.W)))
    
        val prd       = Input(Vec(4, UInt(log2Ceil(n).W)))
        val wdata     = Input(Vec(4, UInt(32.W)))
        val we        = Input(Vec(4, Bool()))
    })
    val rf = RegInit(VecInit.fill(n)(0.U(32.W)))

    for(i <- 0 until 4){
        when(io.we(i)){
            rf(io.prd(i)) := io.wdata(i)
        }
    }
    for(i <- 0 until 4){
        // write first
        {
            val w_hit = VecInit.tabulate(4)(j => io.we(j) && io.prd(j) === io.prj(i)).asUInt
            io.prj_data(i) := Mux(w_hit.orR, Mux1H(w_hit, io.wdata), rf(io.prj(i)))
        }
        {
            val w_hit = VecInit.tabulate(4)(j => io.we(j) && io.prd(j) === io.prk(i)).asUInt
            io.prk_data(i) := Mux(w_hit.orR, Mux1H(w_hit, io.wdata), rf(io.prk(i)))
        }
    }
}
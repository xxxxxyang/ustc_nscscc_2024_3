import chisel3._
import chisel3.util._
import InstPacks._

class Dispatch(n: Int) extends Module {
  val io = IO(new Bundle{
    val inst_pack = Input(Vec(2,new pack_RN))
    //the element of 2 alu
    val elem = Input(Vec(2,UInt(log2Ceil(n).W)))
    //selct which function unit to go
    val inst_valid = Output(Vec(4,Vec(2,Bool())))
  })

  val queue_hit = Wire (Vec(2,UInt(4.W)))
  for(i <- 0 until 2){
    queue_hit(i) := UIntToOH(Mux(io.inst_pack(i).ins_type(2).asBool, Mux(io.elem(1) <= io.elem(2), 1.U(2.W), 0.U(2.W)), io.inst_pack(i).ins_type(1,0))) & Fill(4, io.inst_pack(i).inst_valid)
  }

  for(i <- 0 until 4){
    for(j <- 0 until 2){
        io.inst_valid(i)(j) := queue_hit(j)(i)
    }
  }
}
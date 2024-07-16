import chisel3._
import chisel3.util._
import java.{util => ju}
import Configs._
import Interfaces._
import Util.wrap

class ARAT() extends Module{
    val io = IO(new Bundle{
        val rob         = Input(Vec(2, new ROB_to_ARAT))
        // rename阶段，用于恢复cRAT
        val arat        = Output(Vec(PREG_SIZE, Bool()))
        val head        = Output(UInt(PREG_W.W))
    })
    val arat = RegInit(VecInit.fill(PREG_SIZE)(false.B))
    val head = RegInit(0.U(PREG_W.W))
    // 退休的2个指令是否要写寄存器
    val areg_write = VecInit.tabulate(2)(i=>
        io.rob(i).commit_en && io.rob(i).areg_v)
    for(i <- 0 until 2){
        when(areg_write(i)){
            arat(io.rob(i).preg) := true.B
            arat(io.rob(i).opreg) := false.B
        }
    }
    head := wrap(head +& PopCount(areg_write), PREG_SIZE.U)
    io.arat := arat
    io.head := head
} 

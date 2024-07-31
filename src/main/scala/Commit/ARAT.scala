import chisel3._
import chisel3.util._
import java.{util => ju}
import Configs._
import Interfaces._
import Util.wrap
import Predict_Config._

class ARAT() extends Module{
    val io = IO(new Bundle{
        val rob         = Input(Vec(2, new ROB_to_ARAT))
        // rename阶段，用于恢复cRAT
        val arat        = Output(Vec(PREG_SIZE, Bool()))
        val head        = Output(UInt(PREG_W.W))
        // 恢复ras
        val br_type        = Input(UInt(2.W))
        val pc_cmt         = Input(UInt(32.W))
        val pred_update_en = Input(Bool())
        val real_jump      = Input(Bool())
        val top            = Output(UInt(3.W))
        val ras            = Output(Vec(8, UInt(32.W)))
        val ghr            = Output(UInt(GHR_WIDTH.W))
    })
    val arat = RegInit(VecInit.fill(PREG_SIZE)(false.B))
    val head = RegInit(0.U(PREG_W.W))
    // 退休的2个指令是否要写寄存器
    val rd_write = VecInit.tabulate(2)(i=>
        io.rob(i).commit_en && io.rob(i).rd_valid)
    for(i <- 0 until 2){
        when(rd_write(i)){
            arat(io.rob(i).prd)  := true.B
            arat(io.rob(i).pprd) := false.B
        }
    }
    head := wrap(head +& PopCount(rd_write), PREG_SIZE.U)
    io.arat := arat
    io.head := head
    // ras
    val top = RegInit(7.U(3.W))
    val ras = RegInit(VecInit.fill(8)(PC_RESET))
    when(io.br_type === RET && io.pred_update_en){
        top := top - 1.U
    }.elsewhen(io.br_type(1) && io.pred_update_en){
        top := top + 1.U
        ras(top) := io.pc_cmt
    }
    io.top := top
    io.ras := ras
    //ghr
    val ghr = RegInit(0.U(GHR_WIDTH.W))
    when(io.pred_update_en){
        ghr := ghr(GHR_WIDTH - 2, 0) ## io.real_jump
    }
    io.ghr := ghr
} 

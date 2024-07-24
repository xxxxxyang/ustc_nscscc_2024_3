import chisel3._
import chisel3.util._

class RAT_Item extends Bundle{
    val valid = Bool()
    val areg  = UInt(5.W)
    val ready  = Bool()
}

class CRAT(n:Int) extends Module {
    val io = IO(new Bundle {
        val rj = Input(Vec(2,UInt(5.W)))            //指令的rj寄存器编号
        val rk = Input(Vec(2,UInt(5.W)))            //指令的rk寄存器编号

        val rd = Input(Vec(2,UInt(5.W)))            //指令的rd寄存器编号
        val rd_valid = Input(Vec(2,Bool()))         //指令是否有rd寄存器（是否有写操作）
        val alloc_preg = Input(Vec(2,UInt(log2Ceil(n).W)))//从Freelist中分配的物理寄存器编号

        val prj = Output(Vec(2,UInt(log2Ceil(n).W)))//RAT中rj寄存器的物理寄存器编号
        val prk = Output(Vec(2,UInt(log2Ceil(n).W)))//RAT中rk寄存器的物理寄存器编号
        val pprd = Output(Vec(2,UInt(log2Ceil(n).W)))//RAT中rd寄存器的旧的物理寄存器编号

        val prj_ready = Output(Vec(2,Bool()))       //rj物理寄存器的ready信号
        val prk_ready = Output(Vec(2,Bool()))       //rk物理寄存器的ready信号

        val wake_preg = Input(Vec(4,UInt(log2Ceil(n).W)))  //唤醒的物理寄存器编号
        val wake_valid = Input(Vec(4,Bool()))              //唤醒的物理寄存器是否有效

        val predict_fail = Input(Bool())             //分支预测错误信号
        val arch_rat_valid = Input(Vec(n,Bool())) //ROB维护的architectural RAT中valid位的信息

    })

    val rat = RegInit(VecInit.fill(n)(0.U.asTypeOf(new RAT_Item))) //创建CRAT表

    //更新CRAT
    when(io.predict_fail){
        for(i <- 0 until n){
            rat(i).valid := io.arch_rat_valid(i)   //分支预测失败将用ARAT中的valid位代替CRAT中的valid位
            rat(i).ready := true.B
        }
    }.otherwise{
        for(i <- 0 until 2){
            when(io.rd_valid(i)){
                rat(io.pprd(i)).valid := false.B            //更新CRAT时，将rd之前映射的物理寄存器中valid位置为false
                rat(io.alloc_preg(i)).areg  := io.rd(i)  //将rd映射到新的物理寄存器
                rat(io.alloc_preg(i)).valid := true.B       //将新的物理寄存器的valid位置为true
                rat(io.alloc_preg(i)).ready := false.B      //新的物理寄存器的ready位置为false
            }
        }
        for(i <- 0 until 4){
            when(io.wake_valid(i)){
                rat(io.wake_preg(i)).ready := true.B        //唤醒的物理寄存器的ready位置为true
            }
        }
    }

    //从CRAT中读取rj,rk,rd
    for(i <- 0 until 2){
        for(j <- 0 until n){
            val rj_hit = Wire(Vec(2,UInt(n.W)))         //rj的命中信号，第几位为1表示第几个物理寄存器命中
            val rk_hit = Wire(Vec(2,UInt(n.W)))         //rk的命中信号，第几位为1表示第几个物理寄存器命中
            val rd_hit = Wire(Vec(2,UInt(n.W)))         //rd的命中信号，第几位为1表示第几个物理寄存器命中
            rj_hit(j) := rat(j).valid && (rat(j).areg === io.rj(i))
            rk_hit(j) := rat(j).valid && (rat(j).areg === io.rk(i))
            rd_hit(j) := rat(j).valid && (rat(j).areg === io.rd(i))
        }

        //将命中信号（独热码）转换为物理寄存器编号（UInt）
        io.prj(i) := OHToUInt(rj_hit(i))
        io.prk(i) := OHToUInt(rk_hit(i))        
        io.pprd(i) := OHToUInt(rd_hit(i))
        io.prj_ready(i) := rat(io.prj(i)).ready
        io.prk_ready(i) := rat(io.prk(i)).ready
    }


}
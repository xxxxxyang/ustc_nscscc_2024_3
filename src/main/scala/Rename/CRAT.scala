import chisel3._
import chisel3.util._

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

        val predict_fail = Input(Bool())             //分支预测错误信号
        val arch_rat_valid = Input(Vec(n,Bool())) //ROB维护的architectural RAT中valid位的信息

    })

    import Rat._
    val rat = RegInit(VecInit.fill(n)(0.U.asTypeOf(new rat_t))) //创建CRAT表

    //更新CRAT
    when(predict_fail){
        for(i <- 0 until n){
            rat(i).valid := arch_rat_valid(i)   //分支预测失败将用ARAT中的valid位代替CRAT中的valid位
        }
    }.otherwise{
        for(i <- 0 until 2){
            when(io.rd_valid(i)){
                rat(io.pprd(i)).valid := false.B            //更新CRAT时，将rd之前映射的物理寄存器中valid位置为false
                rat(io.alloc_preg(i)).arch_reg := io.rd(i)  //将rd映射到新的物理寄存器
                rat(io.alloc_preg(i)).valid := true.B       //将新的物理寄存器的valid位置为true
            }
        }
    }

    //从CRAT中读取rj,rk,rd
    val rj_hit = Wire(Vec(2,UInt(n.W)))         //rj的命中信号，第几位为1表示第几个物理寄存器命中
    val rk_hit = Wire(Vec(2,UInt(n.W)))         //rk的命中信号，第几位为1表示第几个物理寄存器命中
    val rd_hit = Wire(Vec(2,UInt(n.W)))         //rd的命中信号，第几位为1表示第几个物理寄存器命中
    for(i <- 0 until 2){
        for(j <- 0 until n){
            rj_hit(i)(j) := rat(j).valid && (rat(j).arch_reg === io.rj(i))
            rk_hit(i)(j) := rat(j).valid && (rat(j).arch_reg === io.rk(i))
            rd_hit(i)(j) := rat(j).valid && (rat(j).arch_reg === io.rd(i))
        }

        //将命中信号（独热码）转换为物理寄存器编号（UInt）
        io.prj(i) := OHToUInt(rj_hit(i))
        io.prk(i) := OHToUInt(rk_hit(i))        
        io.pprd(i) := OHToUInt(rd_hit(i))
    }


}
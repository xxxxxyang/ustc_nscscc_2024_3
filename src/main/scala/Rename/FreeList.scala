import chisel3._
import chisel3.util._

class FreeList(n:Int) extends Module {
    val io = IO(new Bundle {
        val alloc_preg = Output(Vec(2,UInt(log2Ceil(n).W))) //分配的物理寄存器编号
        
        val commit_en = Input(Vec(2,Bool()))
        val commit_pprd_valid = Input(Vec(2,Bool()))
        val commit_pprd = Input(Vec(2,UInt(log2Ceil(n).W)))

        val rd_valid = Input(Vec(2,Bool()))
        val predict_fail = Input(Bool())
        val head_arch = Input(UInt(log2Ceil(n).W))
        val rename_en = Input(Vec(2,Bool()))
        val empty = Output(Bool())
    })

    //空闲队列寄存器队列
    val queue = RegInit(VecInit.tabulate(n)(i => (i+1).asUInt(log2Ceil(n).W)))
    val head = RegInit(1.U(n.W)) //空闲队列头指针,初始指向最低位
    val tail = RegInit(1.U(1.W) ## 0.U((n-1).W)) //空闲队列尾指针,初始指向最高位

    //判断freelist是否为空（head和tail是否间隔1个或相同
    io.empty := ((head & tail) ## ((head(n-2,0) ## head(n-1)) & tail)).orR

    //dequeue 移动head
    val head_sel = Wire(Vec(2,UInt(n.W)))
    var head_temp = head
    io.alloc_preg   := VecInit.fill(2)(0.U(log2Ceil(n).W))
    for(i <- 0 until 2){
        head_sel(i) := head_temp
        head_temp = Mux(io.rename_en(i) && io.rd_valid(i) && !io.empty, head_temp(n-2,0) ## head_temp(n-1),head_temp)
        io.alloc_preg(i) := Mux1H(head_sel(i),queue)
    }
    when(io.predict_fail){
        head := UIntToOH(io.head_arch)
    }.otherwise{
       head := head_temp
    }

    //enqueue 移动tail
    val tail_sel = Wire(Vec(2,UInt(n.W)))
    var tail_temp = tail
    for(i <- 0 until 2){
        tail_temp = Mux(io.commit_pprd_valid(i) && io.commit_en(i),tail_temp(n-2,0) ## tail_temp(n-1),tail_temp)
        tail_sel(i) := tail_temp
        when(io.commit_pprd_valid(i) && io.commit_en(i)){
            queue(OHToUInt(tail_sel(i))) := io.commit_pprd(i)
        }
    }
    tail := tail_temp
}
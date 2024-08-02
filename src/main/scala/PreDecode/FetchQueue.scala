import chisel3._
import chisel3.util._
import Configs._
import Interfaces._
import InstPacks._
import Util._

//lsy
class FetchQueue() extends Module{
    val io = IO(new Bundle{
        val in_pack             = Input(Vec(2, new pack_PD))
        val out_pack            = Output(Vec(2, new pack_PD))
        val out_valid           = Output(Bool())// 要么输出两个，要么不输出
        
        val flush               = Input(Bool())
        val stall               = Input(Bool()) // 停住dequeue
        val full                = Output(Bool())
    })
    val queue = RegInit(
        VecInit.fill(2)(
            VecInit.fill(FQ_SIZE)(
                0.U.asTypeOf(new pack_PD))))
    val head = RegInit(1.U(FQ_SIZE.W)) // 最旧
    val tail = RegInit(1.U(FQ_SIZE.W)) // 最新
    val cur  = RegInit(0.U(1.W)) //当前先加入哪个队列
    
    io.full := (shift1(tail) & head).orR

    // enqueue
    val in_count = PopCount(io.in_pack.map(_.inst_valid))
    val to_insert = VecInit(Mux(io.in_pack(0).inst_valid, io.in_pack(0), io.in_pack(1)), io.in_pack(1))
    when(!io.full){
        when(in_count === 1.U){
            cur := cur ^ 1.U
            for(i <- 0 until FQ_SIZE){
                when(tail(i)){
                    queue(cur)(i) := to_insert(0)
                }
            }
            when(cur.asBool){
                tail := shift1(tail)
            }
        }.elsewhen(in_count === 2.U){
            for(i <- 0 until FQ_SIZE){
                when(tail(i)){
                    queue(cur)(i) := to_insert(0)
                }
                when(Mux(cur.asBool, shift1(tail), tail)(i)){
                    queue(~cur)(i) := to_insert(1)
                }
            }
            tail := shift1(tail)
        }
    }

    // dequeue
    val empty = (tail & head).orR
    io.out_pack  := queue.map(Mux1H(head, _))
    io.out_valid := !empty
    when(!io.out_valid){
        io.out_pack(0).inst_valid := false.B
        io.out_pack(1).inst_valid := false.B
    }
    when(!empty && !io.stall){
        head := shift1(head)
    }

    when(io.flush){
        head := 1.U
        tail := 1.U
    }
} 

// import chisel3._
// import chisel3.util._
// import Configs._
// import Interfaces._
// import InstPacks._
// import Util._

// class IQ_Item_ extends Bundle{
//     val inst            = new pack_DP
//     val prj_ready       = Bool()
//     val prk_ready       = Bool()
//     val prj_wake_by_ld  = Bool()
//     val prk_wake_by_ld  = Bool()
// }
// //lsy
// //每周期，根据insts和insts_valid塞入队列，输出要发射的指令
// //TODO: 推测唤醒
// //n: Issue queue size
// class IssueQueue_(n: Int, ordered: Boolean) extends Module{
//     val io = IO(new Bundle{
//         // 从 Dispatch 接收
//         val insts       = Input(Vec(2, new pack_DP))
//         val insts_valid = Input(Vec(2, Bool()))
//         val prj_ready   = Input(Vec(2, Bool()))
//         val prk_ready   = Input(Vec(2, Bool()))

//         val elem_num    = Output(UInt((log2Ceil(n)+1).W))
//         // 唤醒
//         val wake_preg   = Input(Vec(4, UInt(PREG_W.W)))
//         // 发射
//         val issue_inst  = Output(new pack_DP)
//         val issue_valid = Output(Bool())
//         val to_wake     = Output(UInt(PREG_W.W))

//         val stall       = Input(Bool()) // stall pop
//         val stall_in    = Input(Bool()) // stall insert
//         val flush       = Input(Bool())
//         val full        = Output(Bool())
        
//         //推测唤醒 
//         val ld_mem_prd  = Input(UInt(PREG_W.W))
//         val dcache_miss = Input(Bool())
//     })
//     val queue = RegInit(VecInit.fill(n)(
//         0.U.asTypeOf(new IQ_Item)))
//     val tail  = RegInit(0.U((log2Ceil(n)+1).W))
//     val mask  = RegInit(0.U(n.W)) //mask(i) = (i < tail), mask = (1<<tail) - 1
//     io.full  := mask(n-2)
//     io.elem_num := tail
    
//     // pop
//     val ready  = VecInit.tabulate(n)(i => 
//         mask(i) && queue(i).prj_ready && queue(i).prk_ready && !(
//             (queue(i).prj_wake_by_ld && queue(i).inst.prj === io.ld_mem_prd ||
//              queue(i).prk_wake_by_ld && queue(i).inst.prk === io.ld_mem_prd)
//             && io.dcache_miss
//         ))
//     val is_pop = Wire(Bool())
//     val pop    = Wire(Vec(n, Bool()))
//     if (ordered) {
//         is_pop := ready(0) && !io.stall
//         pop    := VecInit.tabulate(n)(i => (i == 0).B)
//         io.issue_inst  := queue(0).inst
//         io.to_wake := Mux(is_pop && io.issue_inst.rd_valid, io.issue_inst.prd, 0.U)
//     } else {
//         is_pop := ready.asUInt.orR && !io.stall
//         pop    := Mux(is_pop,
//             VecInit(PriorityEncoderOH(ready)),
//             VecInit.fill(n)(false.B))
//         io.issue_inst  := Mux1H(pop, queue).inst
//         io.to_wake := Mux(is_pop && io.issue_inst.rd_valid, io.issue_inst.prd, 0.U)
//     }
    
//     io.issue_valid := is_pop
    
//     val tail_pop   = tail - is_pop
//     val mask_pop   = Mux(is_pop, mask_sub1(mask), mask)
//     val mask_keep  = Mux(is_pop, pop.asUInt - 1.U, mask)
//     val mask_shift = mask_pop & ~mask_keep
    
//     //insert
//     val in_count    = PopCount(io.insts_valid)
//     val insts       = VecInit.tabulate(2)(i => {
//         val item = Wire(new IQ_Item)
//         item.inst      := io.insts(i)
//         item.prj_ready := io.prj_ready(i)
//         item.prk_ready := io.prk_ready(i)
//         item.prj_wake_by_ld := io.ld_mem_prd.orR && (io.insts(i).prj === io.ld_mem_prd)
//         item.prk_wake_by_ld := io.ld_mem_prd.orR && (io.insts(i).prk === io.ld_mem_prd)
//         item
//     })
//     val to_insert   = VecInit(Mux(io.insts_valid(0), insts(0), insts(1)), insts(1))

//     tail := tail_pop + in_count
//     mask := Mux(io.insts_valid.asUInt.andR, mask_add2(mask_pop),
//             Mux(io.insts_valid.asUInt.orR,  mask_add1(mask_pop), mask_pop))
//     when(io.stall_in){
//         tail := tail_pop
//         mask := mask_pop
//     }

//     for(i <- 0 until n){
//         val queue_nxt = Wire(new IQ_Item)
//         queue_nxt := 0.U.asTypeOf(new IQ_Item)
//         when(mask_keep(i)){
//             queue_nxt := queue(i)
//         }.elsewhen(mask_shift(i)){
//             if(i != n-1) queue_nxt := queue(i+1)
//         }.otherwise{
//             when(io.insts_valid.asUInt.orR  && i.U === tail_pop){
//                 queue_nxt := to_insert(0)
//             }
//             when(io.insts_valid.asUInt.andR && i.U === tail_pop + 1.U){
//                 queue_nxt := to_insert(1)
//             }
//         }
//         //wake
//         when(VecInit(io.wake_preg.map(_ === queue_nxt.inst.prj)).asUInt.orR){
//             queue_nxt.prj_ready := true.B
//         }
//         when(VecInit(io.wake_preg.map(_ === queue_nxt.inst.prk)).asUInt.orR){
//             queue_nxt.prk_ready := true.B
//         }
//         when(io.wake_preg(3).orR && io.wake_preg(3) === queue_nxt.inst.prj){
//             queue_nxt.prj_wake_by_ld := true.B
//         }
//         when(io.wake_preg(3).orR && io.wake_preg(3) === queue_nxt.inst.prk){
//             queue_nxt.prk_wake_by_ld := true.B
//         }
//         queue(i) := queue_nxt
//     }
//     when(io.flush){
//         tail := 0.U
//         mask := 0.U
//     }
// } 

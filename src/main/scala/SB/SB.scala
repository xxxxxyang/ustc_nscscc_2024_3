import chisel3._
import chisel3.util._

class sb_t extends Bundle{
    val addr = UInt(32.W)
    val data = UInt(32.W)
    val mem_type = UInt(5.W)
    val wstrb = UInt(4.W)
    val uncache = Bool()
}

class SB(n:Int) extends Module{
    val io = IO(new Bundle{
        //from ex
            val full = Output(Bool())
            val addr_ex = Input(UInt(32.W))
            val mem_type_ex = Input(UInt(5.W))
            val st_data_ex = Input(UInt(32.W))
            val uncache_ex = Input(Bool())
            //from commit
            val st_num = Input(UInt(2.W))
            val flush = Input(Bool())
            val dcache_miss = Input(Bool())
            //write back
            val data_out = Output(UInt(32.W))
            val addr_out = Output(UInt(32.W))
            val uncache_out = Output(Bool())
            val mem_type_out = Output(UInt(5.W))
            val wb_valid = Output(Bool())   //是否写回数据
            //read
            val ld_data_mem      = Output(UInt(32.W))
            val ld_hit           = Output(Vec(4, Bool()))
            val em_stall         = Input(Bool())
    })
    val sb = RegInit(VecInit.fill(n)(0.U.asTypeOf(new sb_t)))
    val head = RegInit(0.U(log2Ceil(n).W))
    val tail = RegInit(0.U(log2Ceil(n).W))
    val wait_num = RegInit(0.U((log2Ceil(n)+1).W))
    val elem_num = RegInit(0.U((log2Ceil(n)+1).W))
    val flush_reg = RegInit(false.B)

    //flush_reg
    when(io.flush){
        flush_reg := true.B
    }.elsewhen(wait_num === 0.U){
        flush_reg := false.B
    }

    //store buffer
    when(flush_reg){
        var start = head + wait_num
        val clear_num = elem_num - wait_num
        for(i <- 0 until n){
            when(i.U < clear_num){
                val clear_idx = start(log2Ceil(n)-1, 0)
                sb(clear_idx).wstrb := 0.U
            }
            start = start + 1.U
        }
    }.elsewhen(!flush_reg && io.mem_type_ex(2)){
        sb(tail).addr := io.addr_ex(31, 2) ## 0.U(2.W)
        sb(tail).data := (io.st_data_ex << (io.addr_ex(1, 0) ## 0.U(3.W)))(31, 0)
        sb(tail).mem_type := io.mem_type_ex
        sb(tail).wstrb := ((UIntToOH(UIntToOH(io.mem_type_ex(1, 0))) - 1.U) << io.addr_ex(1, 0))(3, 0)
        sb(tail).uncache := io.uncache_ex
    }
    //head
    head := head + (io.wb_valid && !io.dcache_miss)
    //tail
    tail := Mux(flush_reg && !wait_num.orR, head + (io.wb_valid && !io.dcache_miss), tail + io.mem_type_ex(2))
    //wait_num
    wait_num := wait_num + io.st_num - (io.wb_valid && !io.dcache_miss)
    //elem_num
    elem_num := Mux(flush_reg && !wait_num.orR, 0.U, elem_num - (io.wb_valid && !io.dcache_miss) + (io.mem_type_ex(2) && !io.full))

    val offset = PriorityEncoder(sb(head).wstrb)
    io.full := (elem_num === n.U) || flush_reg
    io.addr_out := sb(head).addr + offset
    io.data_out := sb(head).data >> (offset ## 0.U(3.W))
    io.mem_type_out := sb(head).mem_type
    io.uncache_out := sb(head).uncache
    io.wb_valid := wait_num.orR

    //Read stage 1 grnerate control signals
    val ld_hit = RegInit(VecInit.fill(4)(VecInit.fill(n)(false.B)))
    val sb_rearrange_reg = RegInit(VecInit.fill(n)(0.U.asTypeOf(new sb_t)))
    val ld_mask = RegInit((UIntToOH(UIntToOH(io.mem_type_ex(1, 0))) - 1.U)(3, 0))
    val ld_hit_mask = RegInit(0.U(4.W))
    val addr_start_reg = RegInit(0.U(2.W))

    val sb_rearrange = VecInit.tabulate(n)(i => sb(tail-1.U-i.U))
    when(!io.em_stall){
        sb_rearrange_reg := sb_rearrange
    }
    when(!io.em_stall){
        ld_hit_mask := Mux(io.mem_type_ex(4), 0xf.U, (15.U << UIntToOH(io.mem_type_ex(1, 0)))(3, 0))
    }
    when(!io.em_stall){
        ld_mask := (UIntToOH(UIntToOH(io.mem_type_ex(1, 0))) - 1.U)(3, 0)
    }
    when(!io.em_stall){
        addr_start_reg := io.addr_ex(1, 0)
    }
    for(j <- 0 until 4){
        when(!io.em_stall){
            ld_hit(j) := VecInit.tabulate(n)(i => !(sb_rearrange(i).addr(31,2) ^ io.addr_ex(31,2)) && sb_rearrange(i).wstrb((io.addr_ex(1,0) + i.U(2.W))(1,0)))
        }
    }

    //Read stage 2 select data
    for(j <- 0 until 4){
        io.ld_hit(j) := ld_hit_mask(j) | (ld_hit(j).asUInt.orR && ld_mask(j))
    }

    val data_byte = Wire(Vec(4, UInt(8.W)))
    for(j <- 0 until 4){
        data_byte(j) := Mux(ld_hit(j).asUInt.orR && ld_mask(j), Mux1H(PriorityEncoderOH(ld_hit.asUInt), sb_rearrange_reg.map(_.data)) >> (addr_start_reg ## 0.U(3.W)), 0.U)
    }
    io.ld_data_mem       := data_byte.asUInt
}

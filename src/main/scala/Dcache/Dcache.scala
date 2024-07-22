import chisel3._
import chisel3.util._


class Dcache_IO extends Bundle{
    // EX
    val addr_EX                 = Input(UInt(32.W))                     // 访存地址
    val wdata_EX                = Input(UInt(32.W))                     // 写数据
    val mem_type_EX             = Input(UInt(5.W))                      // 访存类型
    val store_cmt_EX            = Input(Bool())                         // 写指令提交
    val cacop_en                = Input(Bool())                         // 强序非缓存
    val cacop_op                = Input(UInt(2.W))                      // 强序非缓存操作

    // TC
    val uncache                 = Input(Bool())                         // 非缓存
    val rob_index_TC            = Input(UInt(log2Ceil(ROB_NUM).W))      // ROB索引
    val paddr_TC                = Input(UInt(32.W))                     // 物理地址
    val exception               = Input(Bool())                         // 异常

    // MEM
    val rob_index_CMT           = Input(UInt(log2Ceil(ROB_NUM).W))      // ROB索引
    val rdata_MEM               = Output(UInt(32.W))                    // 读数据

    // AXI
    // read
    val d_rready                = Input(Bool())                         // 总线数据准备好
    val d_rlast                 = Input(Bool())                         // 总线数据最后一个
    val d_rdata                 = Input(UInt(32.W))                     // 总线数据

    val d_rvalid                = Output(Bool())                        // 总线数据请求
    val d_raddr                 = Output(UInt(32.W))                    // 总线地址
    val d_rsize                 = Output(UInt(3.W))                     // 总线数据大小
    val d_rburst                = Output(UInt(2.W))                     // 总线突发类型
    val d_rlen                  = Output(UInt(8.W))                     // 总线数据长度

    // write
    val d_wready                = Input(Bool())                         // 总线写数据准备好
    val d_bvalid                = Input(Bool())                         // 总线写数据请求

    val d_waddr                 = Output(UInt(32.W))                    // 总线写地址
    val d_wdata                 = Output(UInt(32.W))                    // 总线写数据
    val d_wlen                  = Output(UInt(8.W))                     // 总线写数据长度
    val d_wsize                 = Output(UInt(3.W))                     // 总线写数据大小
    val d_wmask                 = Output(UInt(4.W))                     // 总线写数据掩码
    val d_wburst                = Output(UInt(2.W))                     // 总线写数据突发类型
    val d_wvalid                = Output(Bool())                        // 总线写数据请求
    val d_wlast                 = Output(Bool())                        // 总线写数据最后一个
    val d_bready                = Output(Bool())                        // 总线写数据准备好

    // control
    val stall                   = Input(Bool())                         // CPU stall
    val flush                   = Input(Bool())                         // CPU flush

    val cache_miss              = Output(Bool())                        // cache miss
    val has_store               = Output(Bool())                        // 有store指令
}


import Dcache_Config._
class Dcache extends Module{
    val io                      = IO(new Dcache_IO)
    val stall                   = io.stall
    val flush                   = io.flush

    // EX stage
    /* decode */
    val addr_EX                 = io.addr_EX
    val index_EX                = addr_EX(OFFSET_WIDTH + INDEX_WIDTH - 1, OFFSET_WIDTH)

    /* BRAM create */
    val tag_BRAM                = VecInit.fill(2)(Module(new xilinx_simple_dual_port_1_clock_ram_no_change(TAG_WIDTH+1, INDEX_DEPTH)).io)
    val cmem                    = VecInit.fill(2)(Module(new xilinx_simple_dual_port_byte_write_1_clock_ram_read_first(OFFSET_DEPTH, 8, INDEX_DEPTH)).io)

    // EX-TC SegReg
    val cache_miss_stall        = WireDefault(false.B)
    val EX_TC_en                = !(stall || cache_miss_stall)
    val addr_reg_EX_TC          = ShiftRegister(io.addr_EX, 1, EX_TC_en, 0.U)
    val mem_type_reg_EX_TC      = ShiftRegister(io.mem_type_EX, 1, EX_TC_en, 0.U)
    val wdata_reg_EX_TC         = ShiftRegister(io.wdata_EX, 1, EX_TC_en, 0.U)
    val store_cmt_reg_EX_TC     = ShiftRegister(io.store_cmt_EX, 1, EX_TC_en, false.B)
    val cacop_en_reg_EX_TC      = ShiftRegister(io.cacop_en, 1, EX_TC_en, false.B)
    val cacop_op_reg_EX_TC      = ShiftRegister(io.cacop_op, 1, EX_TC_en, 0.U)
    val flush_reg_EX_TC         = ShiftRegister(flush, 1, EX_TC_en || flush, false.B)


    // TC stage
    val store_cmt_TC            = store_cmt_reg_EX_TC
    val cacop_en_TC             = cacop_en_reg_EX_TC
    val cacop_op_TC             = cacop_op_reg_EX_TC
    val flush_TC                = flush_reg_EX_TC
    val addr_TC                 = addr_reg_EX_TC
    val mem_type_TC             = mem_type_reg_EX_TC
    val wdata_TC                = wdata_reg_EX_TC

    val uncache_TC              = Mux(store_cmt_TC, false.B, Mux(flush_TC, true.B, io.uncache))

    // hit

    /* BRAM */
    val tag_addra               = VecInit.fill(2)(0.U(INDEX_WIDTH.W))
    val tag_addrb               = VecInit.fill(2)(0.U(INDEX_WIDTH.W))
    val tag_dina                = VecInit.fill(2)(0.U(TAG_WIDTH+1.W))
    val tag_we                  = WireDefault(VecInit.fill(2)(false.B))

    val cmem_addra              = VecInit.fill(2)(0.U(INDEX_WIDTH.W))
    val cmem_addrb              = VecInit.fill(2)(0.U(INDEX_WIDTH.W))
    val cmem_dina               = VecInit.fill(2)(0.U(8*OFFSET_DEPTH.W))
    val cmem_we                 = WireDefault(VecInit.fill(2)(0.U(OFFSET_DEPTH.W)))

    /* decode */
    val paddr_TC                = io.paddr_TC
    val addr_sel                = WireDefault(FROM_PIPE)
    val tag_TC                  = Mux(store_cmt_TC, addr_TC(31, 32-TAG_WIDTH), paddr_TC(31, 32-TAG_WIDTH))

    for(i <- 0 until 2){
        tag_BRAM(i).addra       := tag_addra
        tag_BRAM(i).addrb       := tag_addrb
        tag_BRAM(i).dina        := tag_dina
        tag_BRAM(i).clka        := clock
        tag_BRAM(i).wea         := tag_we(i)
    }
    for(i <- 0 until 2){
        cmem(i).addra           := index_MEM
        cmem(i).addrb           := Mux(addr_sel === FROM_PIPE, index_RF, index_EX)
        cmem(i).dina            := cmem_dina
        cmem(i).clka            := clock
        cmem(i).wea             := cmem_we(i)
    }

    /* read BRAM */
    tag_addrb                   := Mux(addr_sel === FROM_PIPE, index_EX, index_TC)
    cmem_addrb                  := Mux(addr_sel === FROM_PIPE, index_EX, index_TC)
    val tag_r_TC                = VecInit.tabulate(2)(i => tag_BRAM(i).doutb(TAG_WIDTH-1, 0))
    val valid_r_TC              = VecInit.tabulate(2)(i => tag_BRAM(i).doutb(TAG_WIDTH))
    val cmem_rdata_TC           = VecInit.tabulate(2)(i => cmem(i).doutb)

    /* hit logic */
    val hit_TC                  = VecInit.tabulate(2)(i => valid_r_TC(i) && !(tag_r_TC(i) ^ tag_TC)).asUInt // 2为的UInt,后面可以使用orR UIntToOH 转为index

    // TC-MEM SegReg
    val TC_MEM_en               = !(stall || cache_miss_stall)
    val paddr_reg_TC_MEM        = ShiftRegister(Mux(store_cmt_TC || flush_TC, addr_TC, paddr_TC), 1, TC_MEM_en, 0.U)
    val mem_type_reg_TC_MEM     = ShiftRegister(Mux(mem_type_TC(2) || uncache_TC || store_cmt_TC, mem_type_TC, 0.U), 1, TC_MEM_en, 0.U)
    val wdata_reg_TC_MEM        = ShiftRegister(wdata_TC, 1, TC_MEM_en, 0.U)
    val uncache_reg_TC_MEM      = ShiftRegister(uncache_TC, 1, TC_MEM_en, false.B)
    val hit_reg_TC_MEM          = ShiftRegister(hit_TC, 1, TC_MEM_en, 0.U)
    val tag_r_reg_TC_MEM          = ShiftRegister(tag_r_TC, 1, TC_MEM_en, VecInit(Seq.fill(2)(0.U(TAG_WIDTH.W))))
    val cacop_en_reg_TC_MEM     = ShiftRegister(cacop_en_TC, 1, TC_MEM_en, false.B)
    val cacop_op_reg_TC_MEM     = ShiftRegister(cacop_op_TC, 1, TC_MEM_en, 0.U)
    val flush_reg_TC_MEM        = ShiftRegister(Mux(flush, flush, flush_TC), 1, TC_MEM_en || flush, false.B)
    val cmem_rdata_reg_TC_MEM   = ShiftRegister(cmem_rdata_TC, 1, TC_MEM_en, VecInit(Seq.fill(2)(0.U(8*OFFSET_DEPTH.W))))

    val exception_reg_TC_MEM    = ShiftRegister(io.exception, 1, true.B, false.B)
    val rob_index_reg_TC_MEM    = ShiftRegister(io.rob_index_TC, 1, TC_MEM_en, 0.U)

    // MEM stage
    val addr_MEM                = paddr_reg_TC_MEM
    val mem_type_MEM            = mem_type_reg_TC_MEM
    val wdata_MEM               = wdata_reg_TC_MEM
    val uncache_MEM             = uncache_reg_TC_MEM
    val tag_r_MEM               = tag_r_reg_TC_MEM
    val cacop_en_MEM            = cacop_en_reg_TC_MEM
    val cacop_op_MEM            = cacop_op_reg_TC_MEM
    val flush_MEM               = flush_reg_TC_MEM
    val cmem_rdata_MEM          = cmem_rdata_reg_TC_MEM
    val exception_MEM           = exception_reg_TC_MEM
    val rob_index_MEM           = rob_index_reg_TC_MEM

    /* hit index */
    val hit_MEM                 = hit_reg_TC_MEM
    val hit_oh                  = UIntToOH(hit_MEM)
    val hit_index_MEM           = OHToUInt(hit_oh)
    val cache_hit_MEM           = hit_MEM.orR
    val cache_miss_MEM          = !hit_MEM.orR

    /* decode */
    val tag_MEM                 = addr_MEM(31, 32-TAG_WIDTH)
    val index_MEM               = addr_MEM(INDEX_WIDTH+OFFSET_WIDTH-1, OFFSET_WIDTH)
    val offset_MEM              = addr_MEM(OFFSET_WIDTH-1, 0)

    /* lru */
    val lru_mem                 = RegInit(VecInit.fill(INDEX_DEPTH)(0.U(1.W)))
    val lru_sel                 = lru_mem(index_MEM)
    val lru_miss_upd            = WireDefault(false.B)
    val lru_hit_upd             = WireDefault(false.B)
    when(lru_hit_upd){
        lru_mem(index_MEM) := !hit_index_MEM(1)
    }.elsewhen(lru_miss_upd){
        lru_mem(index_MEM) := !lru_sel
    }

    /* ret buf */
    val ret_buf                 = RegInit(0.U((8*OFFSET_DEPTH).W))                  // 32B
    when(io.d_rready){
        ret_buf                 := Cat(io.d_rdata, ret_buf(8*OFFSET_DEPTH-1, 32))   // 左移拼接rbuf
    }

    /* rdata logic */
    val data_sel                = WireDefault(FROM_RBUF)
    val cmem_rdata_group        = VecInit.tabulate(OFFSET_DEPTH)(i => (0.U(32.W) ## cmem_rdata_MEM(hit_index_MEM))(8*i+31, 8*i))
    val rbuf_group              = VecInit.tabulate(OFFSET_DEPTH)(i => (0.U(32.W) ## ret_buf)(8*i+31, 8*i))
    val rdata_temp              = Mux(data_sel === FROM_RBUF, rbuf_group(offset_MEM), cmem_rdata_group(offset_MEM))
    val rmask                   = WireDefault(0.U(32.W))
    when(mem_type_MEM(1, 0) == 0.U){
        rmask                   := "h0000000f".U
    }.elsewhen(mem_type_MEM(1, 0) === 1.U){
        rmask                   := "h0000ffff".U
    }.elsewhen(mem_type_MEM(1, 0) === 2.U){
        rmask                   := "hffffffff".U
    }.elsewhen(mem_type_MEM(1, 0) === 3.U){
        rmask                   := 0.U
    }
    val rdata                   = rdata_temp & rmask

    /* dirty table logic */
    val dirty_table             = RegInit(VecInit.fill(2)(VecInit.fill(INDEX_DEPTH)(false.B)))
    val dirty_we                = WireDefault(false.B)
    val dirty_clean             = WireDefault(false.B)
    val is_dirty                = dirty_table(lru_sel)(index_MEM)
    val is_store_MEM            = mem_type_MEM(2)
    val is_load_MEM             = !is_store_MEM
    when(dirty_we){
        val write_way           = Mux(cache_hit_MEM, hit_index_MEM, lru_sel)
        dirty_table(write_way)(index_MEM)   := true.B
    }.elsewhen(dirty_clean){
        dirty_table(lru_sel)(index_MEM)     := false.B
    }

    /* write logic */
    tag_dina                    := Mux(cacop_en_MEM, 0.U, true.B ## tag_MEM)
    val block_offset            = offset_MEM ## 0.U(3.W)
    val wmask                   = Mux(is_load_MEM, 0.U((8*OFFSET_DEPTH).W), ((0.U((8*OFFSET_DEPTH-32).W) ## rmask) << block_offset))
    val wdata_refill            = ((0.U((8*OFFSET_DEPTH-32).W) ## wdata_MEM) << block_offset)
    val wmask_byte              = VecInit.tabulate(OFFSET_DEPTH)(i => wmask(8*i)).asUInt    //

    /* write buffer */
    val wrt_buf                     = RegInit(0.U((8*OFFSET_DEPTH+32).W))
    when(wbuf_we){
        when(cacop_en_MEM){
            val cmem_wb_idx = Mux(cacop_op_MEM(1), hit_index_MEM, addr_MEM(0))
            wrt_buf := cmem_rdata_MEM(cmem_wb_idx) ## addr_MEM                      // 需要写的cache line的数据拼接addr
        }.elsewhen(uncache_MEM){
            wrt_buf := 0.U((8*OFFSET_DEPTH-32).W) ## wdata_MEM ## addr_MEM   // 非缓存操作，写入数据拼接addr,前60B补零
        }.otherwise{
            wrt_buf := cmem_rdata_MEM(lru_sel) ## tag_r_MEM(lru_sel) ## addr_MEM(INDEX_WIDTH+OFFSET_WIDTH-1, OFFSET_WIDTH) ## 0.U(OFFSET_WIDTH.W)   // 写入数据拼接tag和index，offset替换为0
        }
    }.elsewhen(io.d_wready && io.d_wvalid){
        wrt_buf := 0.U(32.W) ## wrt_buf(8*OFFSET_DEPTH+32-1, 64) ## wrt_buf(31, 0)  // 总线写入数据拼接addr，实际上是右移操作
    }

}
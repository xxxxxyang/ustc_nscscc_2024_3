/*  version 1.0
    未考虑 强序非缓存 cacop

    容量        8KB
    两路组相联  128组
    cache line  64B
    valid       1
    tag         19
    index       7
    offset      6
 */

import chisel3._
import chisel3.util._
import RAM._

class Icache_IO extends Bundle {
    // IF
    /* input */
    val addr_IF                 = Input(UInt(32.W))         // PC virtual
    val paddr_IF                = Input(UInt(32.W))         // PC physical
    val exception               = Input(Bool())             // 异常
    val rvalid_IF               = Input(Bool())             // CPU读取数据请求

    // RM
    /* output */
    val inst                    = Output(Vec(2, UInt(32.W)))// 指令
    val inst_valid              = Output(Bool())            // 指令有效

    // AXI
    /* input */
    val i_rready                = Input(Bool())             // 总线数据准备好
    val i_rdata                 = Input(UInt(32.W))         // 总线数据
    val i_rlast                 = Input(Bool())             // 总线数据最后一个
    /* output */
    val i_rvalid                = Output(Bool())            // 总线数据请求
    val i_araddr                = Output(UInt(32.W))        // 总线地址
    val i_rsize                 = Output(UInt(3.W))         // 总线数据大小
    val i_rburst                = Output(UInt(2.W))         // 总线突发类型
    val i_rlen                  = Output(UInt(8.W))         // 总线数据长度


    val stall                   = Input(Bool())             // CPU stall
}

import Icache_Config._                                      // 宏定义
class Icache extends Module{
    // io
    val io                      = IO(new Icache_IO)
    val stall                   = io.stall

    // IF
    /* 取index */
    val index_IF                = io.addr_IF(OFFSET_WIDTH + INDEX_WIDTH - 1, OFFSET_WIDTH)
    val addr_sel                = WireDefault(FROM_PIPE)    // 选择BRAM地址为PIPE

    /* TAG BRAM */
    val tag_BRAM                = VecInit.fill(2)(Module(new xilinx_single_port_ram_no_change(TAG_WIDTH + 1, INDEX_DEPTH)).io)      // BRAM 2*20b*128
    val valid_IF                = VecInit.tabulate(2)(i => tag_BRAM(i).douta(TAG_WIDTH))                                            // valid bit
    val tag_IF                  = VecInit.tabulate(2)(i => tag_BRAM(i).douta(TAG_WIDTH - 1, 0))                                     // tag  19bit

    /* Cache MEM */
    val cmem                    = VecInit.fill(2)(Module(new xilinx_single_port_ram_no_change(8 * OFFSET_DEPTH, INDEX_DEPTH)).io)   // BRAM 2*64B*128
    val cmem_line_IF            = VecInit.tabulate(2)(i => cmem(i).douta(8 * OFFSET_DEPTH - 1, 0))                                  // inst_line 64B

    // IF-RM SEGREG
    val paddr_reg               = RegInit(0.U(32.W))        // 物理地址
    val rvalid_reg              = RegInit(false.B)          // 读取数据有效，复位为false
    val cache_miss_RM           = WireDefault(false.B)      // cache miss

    /* Seg Reg 更新 */
    when(!(stall || cache_miss_RM)){
        paddr_reg               := io.paddr_IF
        rvalid_reg              := io.rvalid_IF
    }

    // RM
    /* segreg-> */
    val paddr_RM                = paddr_reg
    val rvalid_RM               = rvalid_reg                // CPU读取请求

    /* 控制信号 */
    val i_rvalid                = WireDefault(false.B)      // 请求总线数据
    val data_sel                = WireDefault(FROM_RBUF)    // 选择给CPU的数据来源为RBUF
    val tag_we                  = WireDefault(VecInit.fill(2)(false.B))
    val cmem_we                 = WireDefault(VecInit.fill(2)(false.B))

    /* addr decode */
    val tag_RM                  = paddr_reg(OFFSET_WIDTH + INDEX_WIDTH + TAG_WIDTH - 1, OFFSET_WIDTH + INDEX_WIDTH)
    val index_RM                = paddr_reg(OFFSET_WIDTH + INDEX_WIDTH - 1, OFFSET_WIDTH)
    val offset_RM               = paddr_reg(OFFSET_WIDTH - 1, 0)

    // AXI data
    /* cnt */
    val cnt                     = RegInit(0.U(4.W))         // 计数器
    val cnt_en                  = WireDefault(false.B)      // 计数器使能
    val cnt_re                  = WireDefault(false.B)      // 计数器复位
    when(cnt_re){
        cnt                     := 0.U
    }.elsewhen(cnt_en){
        cnt                     := cnt + 1.U
    }

    /* return buf */
    val rbuf                    = RegInit(0.U((8 * OFFSET_DEPTH).W))                    // 内存数据返回缓冲区
    val rbuf_inst               = RegInit(0.U(32.W))                                    // 单次内存数据返回缓冲
    val rready                  = ShiftRegister(io.i_rready, 1)                         // 总线数据准备好
    cnt_en                      := rready
    // 从总线读取数据放入return buf
    when(io.i_rready){
        rbuf_inst               := io.i_rdata
    }
    when(rready){
        rbuf                    := rbuf_inst ## rbuf(8 * OFFSET_DEPTH - 1, 32)
    }


    // rdata
    /* tag 比较 */
    /* read/write tag */
    for(i <- 0 until 2){
        tag_BRAM(i).addra       := Mux(addr_sel === FROM_PIPE, index_IF, index_RM)      // 2选1选择器 当在IF阶段时，为状态idle，addr_sel为from_pipe，从BRAM中读取指令，否则即miss，准备向miss的cache line写入指令
        tag_BRAM(i).dina        := true.B ## tag_RM                                     // 写入true ## tag_RM dina为写入数据（即将valid置为1）
        tag_BRAM(i).clka        := clock
        tag_BRAM(i).wea         := tag_we(i)
    }
    /* read/write cmem */
    for(i <- 0 until 2){
        cmem(i).addra           := Mux(addr_sel === FROM_PIPE, index_IF, index_RM)      // 2选1选择器 当在IF阶段时，为状态idle，addr_sel为from_pipe，从BRAM中读取指令，否则即miss，准备向miss的cache line写入指令
        cmem(i).dina            := rbuf                                                 // 写入返回缓冲区数据 dina为写入数据
        cmem(i).clka            := clock
        cmem(i).wea             := cmem_we(i)
    }


    /* hit */
    val cache_hit               = WireDefault(false.B)                                  // cache命中
    val cache_hit_oh            = VecInit.tabulate(2)(i => valid_IF(i) && tag_IF(i) === tag_RM)             // cache line向量的独热码
    val cache_hit_line          = OHToUInt(cache_hit_oh)                                // 命中的cache line序号（cmem[0]或cmem[1]），若未命中则为0
    cache_hit                   := cache_hit_line.orR
    cache_miss_RM               := !cache_hit


    /* cache mem read */
    val cmem_hit_line           = Mux1H(cache_hit_oh, cmem_line_IF)                     // 命中的cache line
    val cmem_hit_group          = VecInit.tabulate(OFFSET_DEPTH/4)(i => if(i == OFFSET_DEPTH/4-1) 0.U(32.W) ## cmem_hit_line(32*i+31, 32*i) else cmem_hit_line(32*i+63, 32*i))
    val cmem_rdata              = cmem_hit_group(offset_RM(OFFSET_WIDTH - 1, 2))        // 从cache line中根据偏移取出两条指令

    /* return buf read */
    val rbuf_hit_group          = VecInit.tabulate(OFFSET_DEPTH/4)(i => if(i == OFFSET_DEPTH/4-1) 0.U(32.W) ## rbuf(32*i+31, 32*i) else rbuf(32*i+63, 32*i))
    val rbuf_rdata              = rbuf_hit_group(offset_RM(OFFSET_WIDTH - 1, 2))        // 从return buf中根据偏移取出两条指令

    /* CPU rdata */
    val rdata                   = VecInit.tabulate(2)(i => (Mux(data_sel === FROM_CMEM, cmem_rdata(32*i+31, 32*i), rbuf_rdata(32*i+31, 32*i))))   // 选择数据来源


    // lru
    val lru                     = RegInit(VecInit.fill(INDEX_DEPTH)(0.U(1.W)))          // lru寄存器堆  128*1bit
    val lru_hit_upd             = WireDefault(false.B)
    val lru_miss_upd            = WireDefault(false.B)
    val lru_sel                 = lru(index_RM)                                         // 选中的cache line序号 0,1

    when(lru_hit_upd){
        lru(index_RM)           := !cache_hit_line                                      // 更新为非命中的cache line序号
    }.elsewhen(lru_miss_upd){
        lru(index_RM)           := !lru_sel                                             // 更新为未被替换的cache line序号
    }


    // FSM
    val s_idle :: s_miss :: s_refill :: s_wait :: Nil = Enum(4)
    val state                   = RegInit(s_idle)
    val inst_valid              = WireDefault(false.B)
    val read_finish             = ShiftRegister(io.i_rlast && rready, 1)                // 从内存替换cache完成 ? rready有必要吗

    switch(state){
        is(s_idle){
            cnt_re              := true.B
            when(io.exception){
                state           := s_idle
            }.elsewhen(rvalid_RM){
                state           := Mux(cache_miss_RM, s_miss, s_idle)
                data_sel        := FROM_CMEM
                addr_sel        := FROM_PIPE
                lru_hit_upd     := cache_hit
                inst_valid      := cache_hit
            }
        }
        is(s_miss){
            i_rvalid            := !read_finish
            state               := Mux(read_finish, s_refill, s_miss)
        }
        is(s_refill){
            addr_sel            := FROM_SEG
            lru_miss_upd        := true.B
            state               := s_wait
            tag_we(lru_sel)     := true.B
            cmem_we(lru_sel)    := true.B
        }
        is(s_wait){
            state               := Mux(stall, s_idle, s_wait)
            data_sel            := FROM_RBUF
            inst_valid          := true.B
        }
    }


    // output
    /* CPU */
    io.inst                     := rdata
    io.inst_valid               := inst_valid

    /* AXI */
    io.i_rvalid                 := i_rvalid
    io.i_araddr                 := tag_RM ## index_RM ## 0.U(OFFSET_WIDTH.W)
    io.i_rburst                 := 1.U
    io.i_rsize                  := 2.U                                          // 读数据宽度
    io.i_rlen                   := (8 * OFFSET_DEPTH/32 - 1).U                  // 读数据长度

}
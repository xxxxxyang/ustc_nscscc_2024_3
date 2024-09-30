# Cache设计

### 基本参数

- 容量：8KB
- 两路组相联
- 128组
- 每个cache line 64B
- tag 19 bit    index 7 bit     offset 6 bit    valid 1 bit
- 一次读两条指令(8B)，存在交叠
- lru 128bit 的寄存器

### 部件

- 2 * 128*64B data 单端口BRAM
- 2 * 128*20b tag  单端口BRAM

### 状态机

- s_idle
  - 判断是否命中
  - 初始化计数器
- s_miss
  - 等待总线发送数据
- s_refill
  - 完成rbuf写，CPU需读的指令就绪
- s_wait
  - 完成cmem更新

时序关系为
* cache hit 时

* cache miss 时

### 输入输出时序

```scala
class Icache_IO extends Bundle {
    // IF
    /* input */
    val addr_IF                 = Input(UInt(32.W))         // PC virtual
    val paddr_IF                = Input(UInt(32.W))         // PC physical
    val exception               = Input(Bool())             // 异常
    val rvalid_IF               = Input(Bool())             // CPU读取数据请求
    val uncache_IF              = Input(Bool())             // 非缓存
    val cacop_en                = Input(Bool())             // 是否有cacop
    val cacop_op                = Input(UInt(2.W))          // cacop操作

    /* output */
    val has_cacop_IF            = Output(Bool())            // 是否有cacop

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
```

* 取指阶段 IF
  * addr_IF 虚拟地址
  * exception 异常
  * rvalid_IF
  * uncache_IF
  * cacop_en
  * cacop_op
  * has_cacop_IF debug信号
* 读RAM阶段 RM
  * paddr_IF 物理地址 从TLB来
  * AXI 总线信号

### Dcache 输入输出时序

```scala
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
```

* EX 阶段 (计算出地址的当周期)
  * addr_EX
  * wdata_EX (store 指令的寄存器)
  * mem_type
  * store_cmt
  * cacop_en
  * cacop_op
* TC (EX 与 读写MEM 中间的周期)
  * paddr
  * uncache
  * rob_index
  * exception
  * has_store
* MEM 
  * AXI
  * rob_index_CMT
  * rdata_MEM
  * cache_miss
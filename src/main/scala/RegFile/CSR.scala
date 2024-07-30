import chisel3._
import chisel3.util._
import exception_code._
import CSR_CODE._

class CSR_REG extends Bundle {
    val crmd        = UInt(32.W)
    val prmd        = UInt(32.W)
    val estat       = UInt(32.W)
    val euen        = UInt(32.W)
    val ecfg        = UInt(32.W)
    val era         = UInt(32.W)
    val badv        = UInt(32.W)
    val eentry      = UInt(32.W)
    val cpuid       = UInt(32.W)
    val save0       = UInt(32.W)
    val save1       = UInt(32.W)
    val save2       = UInt(32.W)
    val save3       = UInt(32.W)
    val llbctl      = UInt(32.W)
    val tlbidx      = UInt(32.W)
    val tlbehi      = UInt(32.W)
    val tlbelo0     = UInt(32.W)
    val tlbelo1     = UInt(32.W)
    val asid        = UInt(32.W)
    val pgdl        = UInt(32.W)
    val pgdh        = UInt(32.W)
    val pgd         = UInt(32.W)
    val tlbrentry   = UInt(32.W)
    val dmw0        = UInt(32.W)
    val dmw1        = UInt(32.W)
    val tid         = UInt(32.W)
    val tcfg        = UInt(32.W)
    val tval        = UInt(32.W)
    val ticlr       = UInt(32.W)
}

class CSR_IO extends Bundle{
    val raddr           = Input(UInt(14.W))
    val rdata           = Output(UInt(32.W))
    val waddr           = Input(UInt(14.W))
    val we              = Input(Bool())
    val wdata           = Input(UInt(32.W))

    // exception and ertn
    val exception           = Input(UInt(8.W))
    val badv_exp            = Input(UInt(32.W))
    val is_ertn             = Input(Bool())
    val pc_exp              = Input(UInt(32.W))
    val eentry_global       = Output(UInt(32.W))
    val tlbrentry_global   = Output(UInt(32.W))

    // interrupt
    val interrupt       = Input(UInt(8.W))
    val ip_int          = Input(Bool())         //核间中断
    val interrupt_vec   = Output(UInt(12.W))

    // mmu
    val asid_global     = Output(UInt(10.W))
    val dmw0_global     = Output(UInt(32.W))
    val dmw1_global     = Output(UInt(32.W))

    // llbit
    val llbit_global    = Output(Bool())
    val llbit_set       = Input(Bool())
    val llbit_clear     = Input(Bool())

    // debug
    val estat_13        = Output(UInt(13.W))
    val csr_reg         = Output(new CSR_REG)
}



class CSR(timer_width: Int) extends Module{
    val io = IO(new CSR_IO)
    val crmd = RegInit(8.U(32.W))
    val prmd = RegInit(0.U(32.W))
    val estat = RegInit(0.U(32.W))
    val euen = RegInit(0.U(32.W))
    val ecfg = RegInit(0.U(32.W))
    val era = RegInit(0.U(32.W))
    val badv = RegInit(0.U(32.W))
    val eentry = RegInit(0.U(32.W))
    val cpuid = RegInit(0.U(32.W))
    val save0 = RegInit(0.U(32.W))
    val save1 = RegInit(0.U(32.W))
    val save2 = RegInit(0.U(32.W))
    val save3 = RegInit(0.U(32.W))
    val llbctl = RegInit(0.U(32.W))
    val tlbidx = RegInit(0.U(32.W))
    val tlbehi = RegInit(0.U(32.W))
    val tlbelo0 = RegInit(0.U(32.W))
    val tlbelo1 = RegInit(0.U(32.W))
    val asid = RegInit(0.U(32.W))
    val pgdl = RegInit(0.U(32.W))
    val pgdh = RegInit(0.U(32.W))
    val pgd = RegInit(0.U(32.W))
    val tlbrentry = RegInit(0.U(32.W))
    val dmw0 = RegInit(0.U(32.W))
    val dmw1 = RegInit(0.U(32.W))
    val tid = RegInit(0.U(32.W))
    val tcfg = RegInit(0.U(32.W))
    val tval = RegInit(0.U(32.W))
    val ticlr = RegInit(0.U(32.W))

    val time_int = RegInit(false.B)

    // CRMD
    val was_tlbr = estat(21, 16) === 0x3f.U
    val is_tlbr = io.exception(6, 0) === 0x3f.U
    when(io.exception(7)){
        crmd := crmd(31, 5) ## Mux(is_tlbr, 1.U(2.W), crmd(4, 3))  ## 0.U(3.W)
    }.elsewhen(io.is_ertn){
        llbctl := llbctl(31,3)
        crmd := crmd(31, 5) ## Mux(was_tlbr, 2.U(2.W), crmd(4, 3)) ## prmd(2, 0)
    }.elsewhen(io.we && io.waddr === CSR_CRMD){
        crmd := 0.U(23.W) ## io.wdata(8, 0)
    }

    // PRMD
    when(io.exception(7)){
        prmd := prmd(31, 3) ## crmd(2, 0)
    }.elsewhen(io.we && io.waddr === CSR_PRMD){
        prmd := 0.U(29.W) ## io.wdata(2, 0)
    }

    // EUEN：扩展部件使能
    when(io.we && io.waddr === CSR_EUEN){
        euen := 0.U(31.W) ## io.wdata(0)
    }

    // ECFG：例外控制
    when(io.we && io.waddr === CSR_ECFG){
        ecfg := 0.U(19.W) ## io.wdata(12, 11) ## 0.U(1.W) ## io.wdata(9, 0)
    }

    // ESTAT: 例外状态
    when(io.exception(7)){
        estat := 0.U(9.W) ## io.exception(6, 0) ## estat(15, 0)
    }.elsewhen(io.we && io.waddr === CSR_ESTAT){
        estat := 0.U(1.W) ## estat(30, 16) ## 0.U(3.W) ## io.ip_int ## time_int ## 0.U(1.W) ## io.interrupt ## io.wdata(1, 0)
    }.otherwise{
        estat := 0.U(1.W) ## estat(30, 16) ## 0.U(3.W) ## io.ip_int ## time_int ## 0.U(1.W) ## io.interrupt ## estat(1, 0)
    }
    io.estat_13 := estat(12, 0)

    // ERA：例外返回地址
    when(io.exception(7)){
        era := io.pc_exp
    }.elsewhen(io.we && io.waddr === CSR_ERA){
        era := io.wdata
    }

    // BADV：出错虚地址
    val badv_exp = Wire(Bool())
    badv_exp := (io.exception(6,0) === TLBR) || (io.exception(6,0) === ADEF) || (io.exception(6,0) === ALE) || ((io.exception(6,0) <= PPI) && (io.exception(6,0) >= PIL))
    when(io.exception(7) && badv_exp){
        badv := io.badv_exp
    }.elsewhen(io.we && io.waddr === CSR_BADV){
        badv := io.wdata
    }

    // EENTRY：例外入口地址
    when(io.we && io.waddr === CSR_EENTRY){
        eentry := io.wdata(31,6) ## 0.U(9.W)
    }

    // CPUID: 处理器编号
    when(io.we && io.waddr === CSR_CPUID){
        cpuid := 0.U(23.W) ## io.wdata(8,0)
    }

    //SAVE0: 数据保存
    when(io.we && io.waddr === CSR_SAVE0){
        save0 := io.wdata
    }

    //SAVE1: 数据保存
    when(io.we && io.waddr === CSR_SAVE1){
        save1 := io.wdata
    }

    //SAVE2: 数据保存
    when(io.we && io.waddr === CSR_SAVE2){
        save2 := io.wdata
    }

    //SAVE3: 数据保存
    when(io.we && io.waddr === CSR_SAVE3){
        save3 := io.wdata
    }

    //LLBCTL: LLbit控制
    when(io.llbit_set){
        llbctl := llbctl(31,1) ## 1.U(1.W)
    }.elsewhen(io.llbit_clear){
        llbctl := llbctl(31,1) ## 0.U(1.W)
    }.elsewhen(io.is_ertn){
        llbctl := llbctl(31,3) ## 0.U(1.W) ## llbctl(1) ## Mux(llbctl(2),llbctl(0),0.U(1.W))
    }.elsewhen(io.we && io.waddr === CSR_LLBCTL){
        llbctl := llbctl(31,3) ## io.wdata(2) ## 0.U(1.W) ## Mux(io.wdata(1),0.U(1.W),llbctl(0))
    }

    //TLBIDX: TLB索引
    //TLBEHI: TLB表项高位
    //TLBELO0: TLB表项低位
    //TLBELO1: TLB表项低位
    //ASID: 地址空间标识符
    //PGDL: 低半地址空间全局目录基址
    when(io.we && io.waddr === CSR_PGDL){
        pgdl := io.wdata(31, 12) ## 0.U(12.W)
    }

    //PGDH: 高半地址空间全局目录基址
    when(io.we && io.waddr === CSR_PGDH){
        pgdh := io.wdata(31, 12) ## 0.U(12.W)
    }

    //PGD: 全局目录基址
    //TLBENTRY: TLB重填例外入口地址
    //DMW0: 数据存储访问控制
    when(io.we && io.waddr === CSR_DMW0){
        dmw0 := io.wdata(31,29) ## 0.U(1.W) ## io.wdata(27,25) ## 0.U(19.W) ## io.wdata(5,3) ## 0.U(2.W) ## io.wdata(0)
    }
    io.dmw0_global := dmw0

    //DMW1: 数据存储访问控制
    when(io.we && io.waddr === CSR_DMW1){
        dmw1 := io.wdata(31,29) ## 0.U(1.W) ## io.wdata(27,25) ## 0.U(19.W) ## io.wdata(5,3) ## 0.U(2.W) ## io.wdata(0)
    }
    io.dmw1_global := dmw1

    //TID: 线程标识符
    when(io.we && io.waddr === CSR_TID){
        tid := io.wdata
    }

    //TCFG: 定时器配置
    when(io.we && io.waddr === CSR_TCFG){
        tcfg := 0.U((32-timer_width).W) ## io.wdata(timer_width-1,0)
    }

    //TVAL: 定时器值
    when(tcfg(0) === 1.U){
        when(tval === 0.U){
            tval := 0.U((32-timer_width).W) ## Mux(tcfg(1),tcfg(timer_width-1,2) ## 0.U(2.W), 0.U(timer_width.W))
        }.otherwise{
            tval := tval - 1.U
        }
    }

    //TICLR: 定时器清零
    val tval_priv = ShiftRegister(tval,1)
    when(io.we && io.waddr === CSR_TICLR && io.wdata(0)){
        time_int := false.B
    }.elsewhen(tcfg(0) === 1.U && tval === 0.U && tval_priv === 1.U){
        time_int := true.B
    }

    switch(io.raddr){
        is(CSR_CRMD)        { io.rdata := crmd }
        is(CSR_PRMD)        { io.rdata := prmd }
        is(CSR_EUEN)        { io.rdata := euen }
        is(CSR_ECFG)        { io.rdata := ecfg }
        is(CSR_ESTAT)       { io.rdata := estat }
        is(CSR_ERA)         { io.rdata := era }
        is(CSR_BADV)        { io.rdata := badv }
        is(CSR_EENTRY)      { io.rdata := eentry }
        is(CSR_CPUID)       { io.rdata := cpuid }
        is(CSR_SAVE0)       { io.rdata := save0 }
        is(CSR_SAVE1)       { io.rdata := save1 }
        is(CSR_SAVE2)       { io.rdata := save2 }
        is(CSR_SAVE3)       { io.rdata := save3 }
        is(CSR_LLBCTL)      { io.rdata := llbctl }
        is(CSR_TLBIDX)      { io.rdata := tlbidx }
        is(CSR_TLBEHI)      { io.rdata := tlbehi }
        is(CSR_TLBELO0)     { io.rdata := tlbelo0 }
        is(CSR_TLBELO1)     { io.rdata := tlbelo1 }
        is(CSR_ASID)        { io.rdata := asid }
        is(CSR_PGDL)        { io.rdata := pgdl }
        is(CSR_PGDH)        { io.rdata := pgdh }
        is(CSR_PGD)         { io.rdata := Mux(badv(31),pgdh,pgdl) }
        is(CSR_TLBRENTRY)   { io.rdata := tlbrentry }
        is(CSR_DMW0)        { io.rdata := dmw0 }
        is(CSR_DMW1)        { io.rdata := dmw1 }
        is(CSR_TID)         { io.rdata := tid }
        is(CSR_TCFG)        { io.rdata := tcfg }
        is(CSR_TVAL)        { io.rdata := tval }
        is(CSR_TICLR)       { io.rdata := ticlr }
    }

    io.csr_reg.crmd := crmd
    io.csr_reg.prmd := prmd
    io.csr_reg.estat := estat
    io.csr_reg.euen := euen
    io.csr_reg.ecfg := ecfg
    io.csr_reg.era := era
    io.csr_reg.badv := badv
    io.csr_reg.eentry := eentry
    io.csr_reg.cpuid := cpuid
    io.csr_reg.save0 := save0
    io.csr_reg.save1 := save1
    io.csr_reg.save2 := save2
    io.csr_reg.save3 := save3
    io.csr_reg.llbctl := llbctl
    io.csr_reg.tlbidx := tlbidx
    io.csr_reg.tlbehi := tlbehi
    io.csr_reg.tlbelo0 := tlbelo0
    io.csr_reg.tlbelo1 := tlbelo1
    io.csr_reg.asid := asid
    io.csr_reg.pgdl := pgdl
    io.csr_reg.pgdh := pgdh
    io.csr_reg.pgd := pgd
    io.csr_reg.tlbrentry := tlbrentry
    io.csr_reg.dmw0 := dmw0
    io.csr_reg.dmw1 := dmw1
    io.csr_reg.tid := tid
    io.csr_reg.tcfg := tcfg
    io.csr_reg.tval := tval
    io.csr_reg.ticlr := ticlr
    io.eentry_global     := eentry
    io.tlbrentry_global  := tlbrentry
    io.interrupt_vec := Mux(!crmd(2), 0.U(12.W), (estat(12, 11) & ecfg(12, 11)) ## (estat(9, 0) & ecfg(9, 0)))
    io.asid_global := asid(9, 0)
    io.llbit_global := llbctl(0)
}
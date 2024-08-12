import chisel3._
import chisel3.util._
import exception_code._
import CSR_CODE._
import CSR_REG._
import TLB_Config._
import TLB_Struct._

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
    val plv_global      = Output(UInt(2.W))
    val crmd_trans      = Output(UInt(6.W))
    val dmw0_global     = Output(UInt(32.W))
    val dmw1_global     = Output(UInt(32.W))
    val tlbehi_global    = Output(UInt(19.W))
    val tlbidx_global   = Output(UInt(log2Ceil(TLB_ENTRY_NUM).W))

    // tlbwr
    val tlbentry_global = Output(new tlb_t)

    // tlbrd
    val tlbentry_in     = Input(new tlb_t)
    val tlbrd_en        = Input(Bool())

    // tlbsrch
    val tlbsrch_en      = Input(Bool())

    // llbit
    val llbit_global    = Output(Bool())
    val llbit_set       = Input(Bool())
    val llbit_clear     = Input(Bool())

    // debug
    val estat_13        = Output(UInt(13.W))
    val csr_reg         = Output(new CSR_REG)
}



class CSR(PALEN: 32, timer_width: Int) extends Module{
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
        eentry := io.wdata(31,6) ## 0.U(6.W)
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
    when(io.tlbsrch_en){
        when(io.wdata(TLB_IDX_WID) === 1.U){//hit
            tlbidx := 0.U(1.W) ## tlbidx(30, TLB_IDX_WID) ## io.wdata(TLB_IDX_WID-1, 0)
        }.otherwise{
            tlbidx := 1.U(1.W) ## tlbidx(30, 0)
        }
    }.elsewhen(io.tlbrd_en){
        tlbidx := !io.tlbentry_in.e ## 0.U(1.W) ## Mux(io.tlbentry_in.e, io.tlbentry_in.ps, 0.U(6.W)) ## tlbidx(23, 0)
    }.elsewhen(io.we && io.waddr === CSR_TLBIDX){
        tlbidx := io.wdata(31) ## 0.U(1.W) ## io.wdata(29, 24) ## 0.U((24-TLB_IDX_WID).W) ## io.wdata(TLB_IDX_WID-1, 0)
    }
    

    //TLBEHI: TLB表项高位
    when(io.exception(7) && (is_tlbr || io.exception(6, 0) >= PIL && io.exception(6, 0) <= PPI)){
        tlbehi := io.badv_exp(31, 13) ## 0.U(13.W)
    }.elsewhen(io.tlbrd_en){
        tlbehi := Mux(io.tlbentry_in.e, io.tlbentry_in.vppn ## 0.U(13.W), 0.U(32.W))
    }.elsewhen(io.we && io.waddr === CSR_TLBEHI){
        tlbehi := io.wdata(31, 13) ## 0.U(13.W)
    }
    

    //TLBELO0: TLB表项低位
    when(io.tlbrd_en){
        tlbelo0 := Mux(io.tlbentry_in.e, tlbelo0(31, PALEN-4) ## io.tlbentry_in.ppn0 ## 0.U(1.W) ## io.tlbentry_in.g ## io.tlbentry_in.mat0 ## io.tlbentry_in.plv0 ## io.tlbentry_in.d0 ## io.tlbentry_in.v0, 0.U(32.W))
    }.elsewhen(io.we && io.waddr === CSR_TLBELO0){
        tlbelo0:= 0.U((36-PALEN).W) ## io.wdata(PALEN-5, 8) ## 0.U(1.W) ## io.wdata(6, 0)
    }

    //TLBELO1: TLB表项低位
    when(io.tlbrd_en){
        tlbelo1 := Mux(io.tlbentry_in.e, tlbelo1(31, PALEN-4) ## io.tlbentry_in.ppn1 ## 0.U(1.W) ## io.tlbentry_in.g ## io.tlbentry_in.mat1 ## io.tlbentry_in.plv1 ## io.tlbentry_in.d1 ## io.tlbentry_in.v1, 0.U(32.W))
    }.elsewhen(io.we && io.waddr === CSR_TLBELO1){
        tlbelo1:= 0.U((36-PALEN).W) ## io.wdata(PALEN-5, 8) ## 0.U(1.W) ## io.wdata(6, 0)
    }

    //ASID: 地址空间标识符
    when(io.tlbrd_en){
        asid := asid(31, 10) ## Mux(io.tlbentry_in.e, io.tlbentry_in.asid, 0.U(10.W))
    }.elsewhen(io.we && io.waddr === CSR_ASID){
        asid := 0.U(22.W) ## io.wdata(9, 0)
    }

    //PGDL: 低半地址空间全局目录基址
    when(io.we && io.waddr === CSR_PGDL){
        pgdl := io.wdata(31, 12) ## 0.U(12.W)
    }

    //PGDH: 高半地址空间全局目录基址
    when(io.we && io.waddr === CSR_PGDH){
        pgdh := io.wdata(31, 12) ## 0.U(12.W)
    }

    //PGD: 全局目录基址
    when(io.we && io.waddr === CSR_PGD){
        pgd := io.wdata(31, 12) ## 0.U(12.W)
    }

    //TLBENTRY: TLB重填例外入口地址
    when(io.we && io.waddr === CSR_TLBRENTRY){
        tlbrentry := io.wdata(31, 6) ## 0.U(6.W)
    }

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
    when(io.we && io.waddr === CSR_TCFG){
        tval := 0.U((32 - timer_width).W) ## io.wdata(timer_width - 1, 2) ## 1.U(2.W)
    }.elsewhen(tcfg(0) === 1.U){
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

    io.rdata := 0.U
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


    //difftest
    when(io.exception(7)){
        io.csr_reg.crmd := crmd(31, 5) ## Mux(is_tlbr, 1.U(2.W), crmd(4, 3))  ## 0.U(3.W)
    }.elsewhen(io.is_ertn){
        io.csr_reg.crmd := crmd(31, 5) ## Mux(was_tlbr, 2.U(2.W), crmd(4, 3)) ## prmd(2, 0)
    }.elsewhen(io.we && io.waddr === CSR_CRMD){
        io.csr_reg.crmd := 0.U(23.W) ## io.wdata(8, 0)
    }.otherwise{
        io.csr_reg.crmd := crmd
    }

    when(io.exception(7)){
        io.csr_reg.prmd := prmd(31, 3) ## crmd(2, 0)
    }.elsewhen(io.we && io.waddr === CSR_PRMD){
        io.csr_reg.prmd := 0.U(29.W) ## io.wdata(2, 0)
    }.otherwise{
        io.csr_reg.prmd := prmd
    }
    
    when(io.we && io.waddr === CSR_EUEN){
        io.csr_reg.euen := 0.U(31.W) ## io.wdata(0)
    }.otherwise{
        io.csr_reg.euen := euen
    }

    when(io.we && io.waddr === CSR_ECFG){
        io.csr_reg.ecfg := 0.U(19.W) ## io.wdata(12, 11) ## 0.U(1.W) ## io.wdata(9, 0)
    }.otherwise{
        io.csr_reg.ecfg := ecfg
    }

    when(io.exception(7)){
        io.csr_reg.estat := 0.U(9.W) ## io.exception(6, 0) ## estat(15, 0)
    }.elsewhen(io.we && io.waddr === CSR_ESTAT){
        io.csr_reg.estat := 0.U(1.W) ## estat(30, 16) ## 0.U(3.W) ## io.ip_int ## time_int ## 0.U(1.W) ## io.interrupt ## io.wdata(1, 0)
    }.otherwise{
        io.csr_reg.estat := 0.U(1.W) ## estat(30, 16) ## 0.U(3.W) ## io.ip_int ## time_int ## 0.U(1.W) ## io.interrupt ## estat(1, 0)
    }

    when(io.exception(7)){
        io.csr_reg.era := io.pc_exp
    }.elsewhen(io.we && io.waddr === CSR_ERA){
        io.csr_reg.era := io.wdata
    }.otherwise{
        io.csr_reg.era := era
    }

    when(io.exception(7) && badv_exp){
        io.csr_reg.badv := io.badv_exp
    }.elsewhen(io.we && io.waddr === CSR_BADV){
        io.csr_reg.badv := io.wdata
    }.otherwise{
        io.csr_reg.badv := badv
    }

    when(io.we && io.waddr === CSR_EENTRY){
        io.csr_reg.eentry := io.wdata(31,6) ## 0.U(6.W)
    }.otherwise{
        io.csr_reg.eentry := eentry
    }

    when(io.we && io.waddr === CSR_CPUID){
        io.csr_reg.cpuid := 0.U(23.W) ## io.wdata(8,0)
    }.otherwise{
        io.csr_reg.cpuid := cpuid
    }

    when(io.we && io.waddr === CSR_SAVE0){
        io.csr_reg.save0 := io.wdata
    }.otherwise{
        io.csr_reg.save0 := save0
    }

    when(io.we && io.waddr === CSR_SAVE1){
        io.csr_reg.save1 := io.wdata
    }.otherwise{
        io.csr_reg.save1 := save1
    }

    when(io.we && io.waddr === CSR_SAVE2){
        io.csr_reg.save2 := io.wdata
    }.otherwise{
        io.csr_reg.save2 := save2
    }

    when(io.we && io.waddr === CSR_SAVE3){
        io.csr_reg.save3 := io.wdata
    }.otherwise{
        io.csr_reg.save3 := save3
    }

    when(io.llbit_set){
        io.csr_reg.llbctl := llbctl(31,1) ## 1.U(1.W)
    }.elsewhen(io.llbit_clear){
        io.csr_reg.llbctl := llbctl(31,1) ## 0.U(1.W)
    }.elsewhen(io.is_ertn){
        io.csr_reg.llbctl := llbctl(31,3) ## 0.U(1.W) ## llbctl(1) ## Mux(llbctl(2),llbctl(0),0.U(1.W))
    }.elsewhen(io.we && io.waddr === CSR_LLBCTL){
        io.csr_reg.llbctl := llbctl(31,3) ## io.wdata(2) ## 0.U(1.W) ## Mux(io.wdata(1),0.U(1.W),llbctl(0))
    }.otherwise{
        io.csr_reg.llbctl := llbctl
    }

    when(io.tlbsrch_en){
        when(io.wdata(TLB_IDX_WID) === 1.U){//hit
            io.csr_reg.tlbidx := 0.U(1.W) ## tlbidx(30, TLB_IDX_WID) ## io.wdata(TLB_IDX_WID-1, 0)
        }.otherwise{
            io.csr_reg.tlbidx := 1.U(1.W) ## tlbidx(30, 0)
        }
    }.elsewhen(io.tlbrd_en){
        io.csr_reg.tlbidx := !io.tlbentry_in.e ## 0.U(1.W) ## Mux(io.tlbentry_in.e, io.tlbentry_in.ps, 0.U(6.W)) ## tlbidx(23, 0)
    }.elsewhen(io.we && io.waddr === CSR_TLBIDX){
        io.csr_reg.tlbidx := io.wdata(31) ## 0.U(1.W) ## io.wdata(29, 24) ## 0.U((24-TLB_IDX_WID).W) ## io.wdata(TLB_IDX_WID-1, 0)
    }.otherwise{
        io.csr_reg.tlbidx := tlbidx
    }

    when(io.exception(7) && (is_tlbr || io.exception(6, 0) >= PIL && io.exception(6, 0) <= PPI)){
        io.csr_reg.tlbehi := io.badv_exp(31, 13) ## 0.U(13.W)
    }.elsewhen(io.tlbrd_en){
        io.csr_reg.tlbehi := Mux(io.tlbentry_in.e, io.tlbentry_in.vppn ## 0.U(13.W), 0.U(32.W))
    }.elsewhen(io.we && io.waddr === CSR_TLBEHI){
        io.csr_reg.tlbehi := io.wdata(31, 13) ## 0.U(13.W)
    }.otherwise{
        io.csr_reg.tlbehi := tlbehi
    }

    when(io.tlbrd_en){
        io.csr_reg.tlbelo0 := Mux(io.tlbentry_in.e, tlbelo0(31, PALEN-4) ## io.tlbentry_in.ppn0 ## 0.U(1.W) ## io.tlbentry_in.g ## io.tlbentry_in.mat0 ## io.tlbentry_in.plv0 ## io.tlbentry_in.d0 ## io.tlbentry_in.v0, 0.U(32.W))
    }.elsewhen(io.we && io.waddr === CSR_TLBELO0){
        io.csr_reg.tlbelo0:= 0.U((36-PALEN).W) ## io.wdata(PALEN-5, 8) ## 0.U(1.W) ## io.wdata(6, 0)
    }.otherwise{
        io.csr_reg.tlbelo0 := tlbelo0
    }

    when(io.tlbrd_en){
        io.csr_reg.tlbelo1 := Mux(io.tlbentry_in.e, tlbelo1(31, PALEN-4) ## io.tlbentry_in.ppn1 ## 0.U(1.W) ## io.tlbentry_in.g ## io.tlbentry_in.mat1 ## io.tlbentry_in.plv1 ## io.tlbentry_in.d1 ## io.tlbentry_in.v1, 0.U(32.W))
    }.elsewhen(io.we && io.waddr === CSR_TLBELO1){
        io.csr_reg.tlbelo1:= 0.U((36-PALEN).W) ## io.wdata(PALEN-5, 8) ## 0.U(1.W) ## io.wdata(6, 0)
    }.otherwise{
        io.csr_reg.tlbelo1 := tlbelo1
    }

    when(io.tlbrd_en){
        io.csr_reg.asid := asid(31, 10) ## Mux(io.tlbentry_in.e, io.tlbentry_in.asid, 0.U(10.W))
    }.elsewhen(io.we && io.waddr === CSR_ASID){
        io.csr_reg.asid := 0.U(22.W) ## io.wdata(9, 0)
    }.otherwise{
        io.csr_reg.asid := asid
    }

    when(io.we && io.waddr === CSR_PGDL){
        io.csr_reg.pgdl := io.wdata(31, 12) ## 0.U(12.W)
    }.otherwise{
        io.csr_reg.pgdl := pgdl
    }

    when(io.we && io.waddr === CSR_PGDH){
        io.csr_reg.pgdh := io.wdata(31, 12) ## 0.U(12.W)
    }.otherwise{
        io.csr_reg.pgdh := pgdh
    }

    when(io.we && io.waddr === CSR_PGD){
        io.csr_reg.pgd := io.wdata(31, 12) ## 0.U(12.W)
    }.otherwise{
        io.csr_reg.pgd := pgd
    }

    when(io.we && io.waddr === CSR_TLBRENTRY){
        io.csr_reg.tlbrentry := io.wdata(31, 6) ## 0.U(6.W)
    }.otherwise{
        io.csr_reg.tlbrentry := tlbrentry
    }

    when(io.we && io.waddr === CSR_DMW0){
        io.csr_reg.dmw0 := io.wdata(31,29) ## 0.U(1.W) ## io.wdata(27,25) ## 0.U(19.W) ## io.wdata(5,3) ## 0.U(2.W) ## io.wdata(0)
    }.otherwise{
        io.csr_reg.dmw0 := dmw0
    }

    when(io.we && io.waddr === CSR_DMW1){
        io.csr_reg.dmw1 := io.wdata(31,29) ## 0.U(1.W) ## io.wdata(27,25) ## 0.U(19.W) ## io.wdata(5,3) ## 0.U(2.W) ## io.wdata(0)
    }.otherwise{
        io.csr_reg.dmw1 := dmw1
    }

    when(io.we && io.waddr === CSR_TID){
        io.csr_reg.tid := io.wdata
    }.otherwise{
        io.csr_reg.tid := tid
    }

    when(io.we && io.waddr === CSR_TCFG){
        io.csr_reg.tcfg := 0.U((32-timer_width).W) ## io.wdata(timer_width-1,0)
    }.otherwise{
        io.csr_reg.tcfg := tcfg
    }

    when(io.we && io.waddr === CSR_TCFG){
        io.csr_reg.tval := 0.U((32 - timer_width).W) ## io.wdata(timer_width - 1, 2) ## 1.U(2.W)
    }.elsewhen(tcfg(0) === 1.U){
        when(tval === 0.U){
            io.csr_reg.tval := 0.U((32-timer_width).W) ## Mux(tcfg(1),tcfg(timer_width-1,2) ## 0.U(2.W), 0.U(timer_width.W))
        }.otherwise{
            io.csr_reg.tval := tval - 1.U
        }
    }.otherwise{
        io.csr_reg.tval := tval
    }

    val tlb_entry = Wire(new tlb_t)
    tlb_entry.vppn := tlbehi(31, 13)
    tlb_entry.ps   := tlbidx(29, 24)
    tlb_entry.g    := tlbelo0(6) && tlbelo1(6)
    tlb_entry.asid := asid(9,0)
    tlb_entry.e    := Mux(estat(21, 16) === 0x3f.U, true.B, !tlbidx(31))
    tlb_entry.ppn0 := tlbelo0(PALEN-5, 8)
    tlb_entry.mat0 := tlbelo0(5, 4)
    tlb_entry.plv0 := tlbelo0(3, 2)
    tlb_entry.d0   := tlbelo0(1)
    tlb_entry.v0   := tlbelo0(0)
    tlb_entry.ppn1 := tlbelo1(PALEN-5, 8)
    tlb_entry.mat1 := tlbelo1(5, 4)
    tlb_entry.plv1 := tlbelo1(3, 2)
    tlb_entry.d1   := tlbelo1(1)
    tlb_entry.v1   := tlbelo1(0)

    io.tlbentry_global := tlb_entry
    io.tlbidx_global := tlbidx(TLB_IDX_WID-1, 0)
    io.tlbehi_global := tlbehi(31, 13)
    io.asid_global := asid(9, 0)
    io.csr_reg.ticlr := ticlr
    io.eentry_global     := eentry
    io.tlbrentry_global  := tlbrentry
    io.interrupt_vec := Mux(!crmd(2), 0.U(12.W), (estat(12, 11) & ecfg(12, 11)) ## (estat(9, 0) & ecfg(9, 0)))
    io.asid_global := asid(9, 0)
    io.llbit_global := llbctl(0)
    io.plv_global := crmd(1, 0)
    io.crmd_trans := crmd(8, 3)
}
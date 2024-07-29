import chisel3._
import chisel3.util._
import Configs._
import Interfaces._
import InstPacks._
import Util._
import control_signal._

class CPU extends Module {
    val io = IO(new Bundle{
        val araddr  = Output(UInt(32.W))
        val arburst = Output(UInt(2.W))
        val arid    = Output(UInt(4.W))
        val arlen   = Output(UInt(8.W))  
        val arready = Input(Bool())
        val arsize  = Output(UInt(3.W))
        val arvalid = Output(Bool())

        val awaddr  = Output(UInt(32.W))
        val awburst = Output(UInt(2.W))
        val awid    = Output(UInt(4.W))
        val awlen   = Output(UInt(8.W))
        val awready = Input(Bool())
        val awsize  = Output(UInt(3.W))
        val awvalid = Output(Bool())

        val bid     = Input(UInt(4.W))
        val bready  = Output(Bool())
        val bresp   = Input(UInt(2.W))
        val bvalid  = Input(Bool())

        val rdata   = Input(UInt(32.W))
        val rid     = Input(UInt(4.W))
        val rlast   = Input(Bool())
        val rready  = Output(Bool())
        val rresp   = Input(UInt(2.W))
        val rvalid  = Input(Bool())

        val wdata   = Output(UInt(32.W))
        val wlast   = Output(Bool())
        val wready  = Input(Bool())
        val wstrb   = Output(UInt(4.W))
        val wvalid  = Output(Bool())
    })
    // PF 
    val pc = Module(new PC)
    val predict = Module(new Predict)
    // IF
    val icache = Module(new Icache)
    // PD
    val pd = Module(new PreDecode)
    val fq = Module(new FetchQueue)
    // ID
    val decode = Seq(Module(new Decoder), Module(new Decoder))
    val freelist = Module(new FreeList(PREG_SIZE))
    // Rename
    val rename = Module(new Rename(PREG_SIZE))
    val dp = Module(new Dispatch(IQ_SIZE(1)))
    // Issue
    val iq0 = Module(new IssueQueue(IQ_SIZE(0), true))
    val iq1 = Module(new IssueQueue(IQ_SIZE(1), false))
    val iq2 = Module(new IssueQueue(IQ_SIZE(2), false))
    val iq3 = Module(new IssueQueue(IQ_SIZE(3), true))
    // RF
    val rf = Module(new RegFile)
    // EX
    val alu1 = Module(new ALU)
    val cnt  = Module(new CounterModule)

    val alu2 = Module(new ALU)
    val br   = Module(new Branch)
    // WB
    val rob    = Module(new ROB)
    val arat   = Module(new ARAT)
    val bypass = Module(new Bypass)

    //global signals
    val predict_fail  = Wire(Bool())
    val branch_target = Wire(UInt(32.W))
    val iq_full       = Wire(Bool())
    val dcache_miss_hazard = Wire(Bool())
    val sb_full_hazard     = Wire(Bool())
    val sb_cmt_hazard      = Wire(Bool())
    // ======================================
    // pre fetch
    val icache_waiting = !icache.io.inst_valid || icache.io.has_cacop_IF
    pc.io.stall         := fq.io.full || icache_waiting
    pc.io.pred_jump     := predict.io.pred_jump
    pc.io.pred_npc      := predict.io.pred_npc
    pc.io.predict_fail  := predict_fail
    pc.io.branch_target := branch_target
    pc.io.pd_fix_en     := pd.io.pd_fix_en
    pc.io.pd_fix_target := pd.io.pd_fix_target
    pc.io.idle_start    := false.B 
    pc.io.idle_intr     := false.B

    predict.io.pc       := pc.io.pc
    predict.io.npc      := pc.io.npc
    predict.io.pd_fix_en    := pd.io.pd_fix_en
    predict.io.pd_fix_is_bl := pd.io.pd_fix_is_bl
    predict.io.pd_fix_pc    := pd.io.pd_fix_pc

    val insts_PF = VecInit.tabulate(2)(i => {
        val inst = Wire(new pack_PF)
        inst.inst_valid := pc.io.inst_valid(i)
        inst.pc         := pc.io.pc + (i*4).U
        inst.pred_valid := predict.io.pred_valid
        inst.pred_jump  := predict.io.pred_jump
        inst.pred_npc   := predict.io.pred_npc
        inst.br_cnt     := predict.io.br_cnt(i)
        inst.exception  := pc.io.exception
        inst
    })

    // PF-IF
    val insts_PF_IF = reg1(insts_PF,
        fq.io.full || icache_waiting, //pi stall
        predict_fail || !fq.io.full && pd.io.pd_fix_en) //pi flush
    // instruction fetch ------------------------
    icache.io.rvalid_IF  := true.B
    icache.io.addr_IF    := pc.io.pc
    icache.io.paddr_IF   := pc.io.pc // todo: mmu
    icache.io.exception  := 0.U
    icache.io.uncache_IF := false.B
    icache.io.cacop_en   := false.B
    icache.io.cacop_op   := 0.U
    icache.io.stall      := fq.io.full
    val insts_IF = VecInit.tabulate(2)(i =>
        pack_IF(insts_PF_IF(i), icache.io.inst(i)))

    // IF-PD
    val insts_IF_PD = reg1(insts_IF,
        fq.io.full, //ip stall
        predict_fail || !fq.io.full && (pd.io.pd_fix_en || !icache.io.inst_valid)) //ip flush

    // pre decode -------------------------------
    pd.io.insts := insts_IF_PD
    fq.io.in_pack := pd.io.insts_PD
    fq.io.stall   := rob.io.full || iq_full || freelist.io.empty
    fq.io.flush   := predict_fail

    // instruction decode -----------------------
    decode(0).io.inst_FQ := fq.io.out_pack(0)
    decode(1).io.inst_FQ := fq.io.out_pack(1)
    val insts_ID = VecInit.tabulate(2)(i => decode(i).io.inst_ID)

    freelist.io.rd_valid     := insts_ID.map(_.rd_valid)
    freelist.io.rename_en    := VecInit.fill(2)(fq.io.out_valid && !fq.io.stall)
    freelist.io.predict_fail := reg1(predict_fail)
    freelist.io.head_arch    := arat.head
    freelist.io.commit_en         := rob.io.arat.map(_.commit_en)
    freelist.io.commit_pprd_valid := rob.io.arat.map(_.rd_valid)
    freelist.io.commit_pprd       := rob.io.arat.map(_.pprd)

    // ID-RN
    val dr_stall = rob.io.full || iq_full
    val dr_flush = predict_fail || (!dr_stall && freelist.io.empty)
    val insts_ID_RN   = reg1(insts_ID, dr_stall, dr_flush)
    val alloc_preg_RN = reg1(freelist.io.alloc_preg, dr_stall, dr_flush)

    // rename ------------------------------------
    rename.io.rj             := insts_ID_RN.map(_.rj)
    rename.io.rk             := insts_ID_RN.map(_.rk)
    rename.io.rd             := insts_ID_RN.map(_.rd)
    rename.io.rd_valid       := insts_ID_RN.map(_.rd_valid)
    rename.io.rename_en      := insts_ID_RN.map(_.inst_valid && !dr_stall)
    rename.io.predict_fail   := reg1(predict_fail)
    rename.io.arch_rat_valid := arat.io.arat
    rename.io.alloc_preg     := alloc_preg_RN
    //todo: wake
    
    val insts_RN = VecInit.tabulate(2)(i =>
        pack_RN(insts_ID_RN(i), rename.io.prj(i), rename.io.prk(i), rename.io.prd(i), rename.io.pprd(i)))

    dp.io.inst_pack := insts_RN
    dp.io.elem      := VecInit(iq1.io.elem_num, iq2.io.elem_num)
    
    rob.io.dp_valid := insts_RN(0).inst_valid
    rob.io.dp := VecInit.tabulate(2)(i =>  {
        val item = Wire(new DP_to_ROB)
        val inst = insts_RN(i)
        item.exception  := inst.exception
        item.rd         := inst.rd
        item.rd_valid   := inst.rd_valid
        item.prd        := inst.prd
        item.pprd       := inst.pprd
        item.pc         := inst.pc
        item.is_store   := inst.mem_type(2)
        item.is_br      := inst.br_type.orR
        item.br_type    := {
            import Predict_Config._
            val typ = WireDefault(ELSE)
            when(inst.br_type === BR_BL){ typ := BL }
            .elsewhen(inst.br_type === BR_JIRL){
                when     (inst.rd === 1.U){ typ := ICALL }
                .elsewhen(inst.rj === 1.U){ typ := RET }
            }
            typ
        }
        item.priv_vec   := inst.priv_vec
        item.inst
    })
    rob.io.stall := dr_stall

    val insts_DP = VecInit.tabulate(2)(i => pack_DP(insts_RN(i), rob.io.rob_index(i)))

    // issue -------------------------------------
    iq_full := iq0.io.full || iq1.io.full || iq2.io.full || iq3.io.full

    iq0.io.insts        := insts_DP
    iq0.io.insts_valid  := dp.io.inst_valid(0)
    iq0.io.prj_ready    := rename.io.prj_ready
    iq0.io.prk_ready    := rename.io.prk_ready
    iq0.io.stall        := ir_reg0.io.stall || reg1(ir_reg0.io.stall)
    iq0.io.stall_in     := iq_full || rob.io.full
    iq0.io.flush        := predict_fail

    iq1.io.insts        := insts_DP
    iq1.io.insts_valid  := dp.io.inst_valid(1)
    iq1.io.prj_ready    := rename.io.prj_ready
    iq1.io.prk_ready    := rename.io.prk_ready
    iq1.io.stall        := false.B
    iq1.io.stall_in     := iq_full || rob.io.full
    iq1.io.flush        := predict_fail
    

    iq2.io.insts        := insts_DP
    iq2.io.insts_valid  := dp.io.inst_valid(2)
    iq2.io.prj_ready    := rename.io.prj_ready
    iq2.io.prk_ready    := rename.io.prk_ready
    iq2.io.stall        := false.B
    iq2.io.stall_in     := iq_full || rob.io.full
    iq2.io.flush        := predict_fail

    iq3.io.insts        := insts_DP
    iq3.io.insts_valid  := dp.io.inst_valid(3)
    iq3.io.prj_ready    := rename.io.prj_ready
    iq3.io.prk_ready    := rename.io.prk_ready
    iq3.io.stall        := ir_reg3.io.stall || reg1(ir_reg3.io.stall)
    iq3.io.stall_in     := iq_full || rob.io.full
    iq3.io.flush        := predict_fail

    val iq_self_wake_preg = VecInit(Mux(!mdu.io.busy, md_ex2_ex3_reg.io.inst_pack_EX2.prd, 0.U),sel1.io.wake_preg, sel2.io.wake_preg, re_reg3.io.inst_pack_EX.prd)
    val iq_mutual_wake_preg = VecInit(Mux(!mdu.io.busy, md_ex2_ex3_reg.io.inst_pack_EX2.prd, 0.U),ir_reg1.io.inst_pack_RF.prd,ir_reg2.io.inst_pack_RF.prd,Mux(!dcache.io.cache_miss_MEM(4), re_reg3.io.inst_pack_EX.prd, 0.U))

    iq0.io.wake_preg := VecInit(iq_self_wake_preg(0),iq_mutual_wake_preg(1),iq_mutual_wake_preg(2),iq_mutual_wake_preg(3))
    iq1.io.wake_preg := VecInit(iq_mutual_wake_preg(0),iq_self_wake_preg(1),iq_self_wake_preg(2),iq_mutual_wake_preg(3))
    iq2.io.wake_preg := VecInit(iq_mutual_wake_preg(0),iq_self_wake_preg(1),iq_self_wake_preg(2),iq_mutual_wake_preg(3))
    iq3.io.wake_preg := VecInit(iq_mutual_wake_preg(0),iq_mutual_wake_preg(1),iq_mutual_wake_preg(2),iq_self_wake_preg(3))
    //未完成的变量赋值
    dcache_miss_hazard := dcache.io.cache_miss
    sb_full_hazard := sb.io.full && re_reg3.io.is_store
    sb_cmt_hazard := sb.io.wb_valid && ir_reg3.io.is_ls
    val ir_reg3.io.stall := dcache_miss_hazard || sb_full_hazard || sb_cmt_hazard
    val ir_reg0.io.stall := mdu.io.busy
    

}
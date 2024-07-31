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
    val csr = Module(new CSR(30))
    // EX
    val mdu = Module(new MDU)

    val alu1 = Module(new ALU)
    val cnt  = Module(new CounterModule)

    val alu2 = Module(new ALU)
    val br   = Module(new Branch)

    val dcache = Module(new Dcache)
    val sb = Module(new SB(SB_NUM))
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

    val ir0_stall        = mdu.io.busy 
    iq0.io.insts        := insts_DP
    iq0.io.insts_valid  := dp.io.inst_valid(0)
    iq0.io.prj_ready    := rename.io.prj_ready
    iq0.io.prk_ready    := rename.io.prk_ready
    iq0.io.stall        := ir0_stall || reg1(ir0_stall)
    iq0.io.stall_in     := iq_full || rob.io.full
    iq0.io.flush        := predict_fail
    val inst_iq0 = iq0.io.issue_inst
    inst_iq0.inst_valid := iq0.io.issue_valid

    iq1.io.insts        := insts_DP
    iq1.io.insts_valid  := dp.io.inst_valid(1)
    iq1.io.prj_ready    := rename.io.prj_ready
    iq1.io.prk_ready    := rename.io.prk_ready
    iq1.io.stall        := false.B
    iq1.io.stall_in     := iq_full || rob.io.full
    iq1.io.flush        := predict_fail
    val inst_iq1 = iq1.io.issue_inst
    inst_iq1.inst_valid := iq1.io.issue_valid

    iq2.io.insts        := insts_DP
    iq2.io.insts_valid  := dp.io.inst_valid(2)
    iq2.io.prj_ready    := rename.io.prj_ready
    iq2.io.prk_ready    := rename.io.prk_ready
    iq2.io.stall        := false.B
    iq2.io.stall_in     := iq_full || rob.io.full
    iq2.io.flush        := predict_fail
    val inst_iq2 = iq2.io.issue_inst
    inst_iq2.inst_valid := iq2.io.issue_valid

    dcache_miss_hazard  := dcache.io.cache_miss
    sb_full_hazard      := sb.io.full && inst_ex3.mem_type(2)
    sb_cmt_hazard       := sb.io.wb_valid && inst_rf3.mem_type(4)
    val ir3_stall        = dcache_miss_hazard || sb_full_hazard || sb_cmt_hazard
    iq3.io.insts        := insts_DP
    iq3.io.insts_valid  := dp.io.inst_valid(3)
    iq3.io.prj_ready    := rename.io.prj_ready
    iq3.io.prk_ready    := rename.io.prk_ready
    iq3.io.stall        := ir3_stall || reg1(ir3_stall)
    iq3.io.stall_in     := iq_full || rob.io.full
    iq3.io.flush        := predict_fail
    val inst_iq3 = iq3.io.issue_inst
    inst_iq3.inst_valid := iq3.io.issue_valid

    val iq_self_wake_preg = VecInit(Mux(!mdu.io.busy, inst_ex0_2.prd, 0.U), iq1.io.to_wake, iq2.io.to_wake, inst_ex3.prd)
    val iq_mutual_wake_preg = VecInit(Mux(!mdu.io.busy, inst_ex0_2.prd, 0.U), inst_rf1.prd, inst_rf2.prd, Mux(!dcache_miss_hazard, inst_ex3.prd, 0.U))

    iq0.io.wake_preg := VecInit(iq_self_wake_preg(0),iq_mutual_wake_preg(1),iq_mutual_wake_preg(2),iq_mutual_wake_preg(3))
    iq1.io.wake_preg := VecInit(iq_mutual_wake_preg(0),iq_self_wake_preg(1),iq_self_wake_preg(2),iq_mutual_wake_preg(3))
    iq2.io.wake_preg := VecInit(iq_mutual_wake_preg(0),iq_self_wake_preg(1),iq_self_wake_preg(2),iq_mutual_wake_preg(3))
    iq3.io.wake_preg := VecInit(iq_mutual_wake_preg(0),iq_mutual_wake_preg(1),iq_mutual_wake_preg(2),iq_self_wake_preg(3))
    //未完成的变量赋值

    // IS-RF
    val inst_rf0 = reg1(inst_iq0, ir0_stall, predict_fail)
    val inst_rf1 = reg1(inst_iq1, false.B,   predict_fail)
    val inst_rf2 = reg1(inst_iq2, false.B,   predict_fail)
    val inst_rf3 = reg1(inst_iq3, ir3_stall, predict_fail)
    val prj_data_rf3 = reg_fw(rf.io.prj_data(3),
        bypass.io.forward_prj_en(3), bypass.io.forward_prj_data(3),
        ir3_stall, predict_fail)
    val prk_data_rf3 = reg_fw(rf.io.prk_data(3),
        bypass.io.forward_prk_en(3), bypass.io.forward_prk_data(3),
        ir3_stall, predict_fail)
    
    // regfile read -------------------------
    rf.io.prj := VecInit(inst_rf0.prj, inst_rf1.prj, inst_rf2.prj, inst_iq3.prj)
    rf.io.prk := VecInit(inst_rf0.prk, inst_rf1.prk, inst_rf2.prk, inst_iq3.prk)
    
    csr.io.raddr := inst_rf0.imm(13, 0)

    //todo: RF stage for LS pipeline (3)
    // RF-EX
    val inst_ex0 = reg1(inst_rf0, mdu.io.busy, predict_fail)
    val inst_ex1 = reg1(inst_rf1, false.B, predict_fail)
    val inst_ex2 = reg1(inst_rf2, false.B, predict_fail)
    val re3_stall = dcache_miss_hazard || sb_full_hazard 
    val re3_flush = predict_fail || !re3_stall && sb_cmt_hazard
    val inst_ex3 = reg1(inst_rf3, re3_stall, re3_flush)

    val csr_rdata_ex = reg1(csr.io.rdata, mdu.io.busy, predict_fail)

    val prj_data_ex0 = reg_fw(rf.io.prj_data(0),
        bypass.io.forward_prj_en(0), bypass.io.forward_prj_data(0),
        mdu.io.busy, predict_fail)
    val prk_data_ex0 = reg_fw(rf.io.prk_data(0),
        bypass.io.forward_prk_en(0), bypass.io.forward_prk_data(0),
        mdu.io.busy, predict_fail)
    val prj_data_ex1 = reg_fw(rf.io.prj_data(1),
        bypass.io.forward_prj_en(1), bypass.io.forward_prj_data(1),
        false.B, predict_fail)
    val prk_data_ex1 = reg_fw(rf.io.prk_data(1),
        bypass.io.forward_prk_en(1), bypass.io.forward_prk_data(1),
        false.B, predict_fail)
    val prj_data_ex2 = reg_fw(rf.io.prj_data(2),
        bypass.io.forward_prj_en(2), bypass.io.forward_prj_data(2),
        false.B, predict_fail)
    val prk_data_ex2 = reg_fw(rf.io.prk_data(2),
        bypass.io.forward_prk_en(2), bypass.io.forward_prk_data(2),
        false.B, predict_fail)
    // val prj_data_ex3 = 
    // execute -------------------------
    //0
    mdu.io.op := inst_ex0.alu_op
    mdu.io.src1 := prj_data_ex0
    mdu.io.src2 := prk_data_ex0

    rob.io.ex.priv_vec      := inst_ex0.priv_vec(9, 0)
    rob.io.ex.csr_addr      := inst_ex0.imm(13, 0)
    rob.io.ex.tlb_entry     := 0.U // todo: MMU
    rob.io.ex.invtlb_op     := inst_ex0.imm(4, 0)
    rob.io.ex.invtlb_asid   := prj_data_ex0(9, 0)
    rob.io.ex.invtlb_vaddr  := prk_data_ex0
    
    //todo: tlbsrch
    val csr_wdata_ex = Mux(inst_ex0.priv_vec(3), csr_rdata_ex, //ERTN
        Mux(inst_ex0.priv_vec(2), 
            prj_data_ex0 & prk_data_ex0 | ~prj_data_ex0 & csr_rdata_ex, //CSRXCHG
            prk_data_ex0)) //CSRWR

    val inst_ex0_1     = reg1(inst_ex0,       mdu.io.busy, predict_fail)
    val inst_ex0_2     = reg1(inst_ex0_1,     mdu.io.busy, predict_fail)
    val csr_rdata_ex_1 = reg1(csr_rdata_ex,   mdu.io.busy, predict_fail)
    val csr_rdata_ex_2 = reg1(csr_rdata_ex_1, mdu.io.busy, predict_fail)
    val csr_wdata_ex_1 = reg1(csr_wdata_ex,   mdu.io.busy, predict_fail)
    val csr_wdata_ex_2 = reg1(csr_wdata_ex_1, mdu.io.busy, predict_fail)
    
    //1
    alu1.io.alu_op := inst_ex1.alu_op
    alu1.io.src1 := Mux(inst_ex1.alu_rs1_sel === RS1_PC, inst_ex1.pc, prj_data_ex1)
    alu1.io.src2 := MuxLookup(inst_ex1.alu_rs2_sel, 0.U)(Seq(
        RS2_REG  -> prk_data_ex1,
        RS2_IMM  -> inst_ex1.imm,
        RS2_CNTH -> cnt.io.cnt(63, 32),
        RS2_CNTL -> cnt.io.cnt(31, 0)))
    
    //2
    alu2.io.alu_op := inst_ex2.alu_op
    alu2.io.src1 := Mux(inst_ex2.alu_rs1_sel === RS1_PC, inst_ex2.pc, prj_data_ex2)
    alu2.io.src2 := MuxLookup(inst_ex2.alu_rs2_sel, 0.U)(Seq(
        RS2_REG  -> prk_data_ex2,
        RS2_IMM  -> inst_ex2.imm,
        RS2_FOUR -> 4.U))
    br.io.br_type := inst_ex2.br_type
    br.io.src1 := prj_data_ex2
    br.io.src2 := prk_data_ex2
    br.io.pc   := inst_ex2.pc
    br.io.imm  := inst_ex2.imm
    br.io.pred_jump := inst_ex2.pred_jump
    br.io.pred_npc  := inst_ex2.pred_npc

    // EX-WB
    val inst_wb0 = reg1(inst_ex0_2, false.B, predict_fail || mdu.io.busy)
    val inst_wb1 = reg1(inst_ex1, false.B, predict_fail)
    val inst_wb2 = reg1(inst_ex2, false.B, predict_fail)
    val inst_wb3 = reg1(inst_ex3, false.B, predict_fail || dcache_miss_hazard)
    
    //0
    val csr_wdata_wb = reg1(csr_wdata_ex_2, false.B, predict_fail || mdu.io.busy)
    val md_out_wb = reg1(Mux(inst_ex0_2.priv_vec(0), csr_rdata_ex_2, 
        Mux(inst_ex0_2.alu_op(2), mdu.io.div_res, mdu.io.mul_res)))
    //1
    val alu_out_wb1      = reg1(alu1.io.alu_out,     false.B, predict_fail)
    //2
    val alu_out_wb2      = reg1(alu2.io.alu_out,     false.B, predict_fail)
    val predict_fail_wb  = reg1(br.io.predict_fail,  false.B, predict_fail)
    val branch_target_wb = reg1(br.io.branch_target, false.B, predict_fail)
    val branch_en_wb     = reg1(br.io.branch_en,     false.B, predict_fail)

    // write back -------------------------
    rf.io.prd   := VecInit(inst_wb0.prd, inst_wb1.prd, inst_wb2.prd, inst_wb3.prd)
    rf.io.we    := VecInit(inst_wb0.rd_valid, inst_wb1.rd_valid, inst_wb2.rd_valid, inst_wb3.rd_valid && exception_wb3)
    rf.io.wdata := VecInit(md_out_wb, alu_out_wb1, alu_out_wb2, mem_rdata_wb)
    bypass.io.prj_ex := VecInit(inst_ex0.prj, inst_ex1.prj, inst_ex2.prj, inst_rf3.prj)
    bypass.io.prk_ex := VecInit(inst_ex0.prk, inst_ex1.prk, inst_ex2.prk, inst_rf3.prk)
    bypass.io.prd_wb := rf.io.prd
    bypass.io.rd_valid_wb := rf.io.we
    bypass.io.prd_wdata_wb := rf.io.wdata
    
}
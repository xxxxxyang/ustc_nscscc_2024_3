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
    val arb = Module(new Arbiter_AXI)
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
    val mdu = Module(new MDU)

    val alu1 = Module(new ALU)
    val cnt  = Module(new CounterModule)

    val alu2 = Module(new ALU)
    val br   = Module(new Branch)

    val dcache = Module(new Dcache)
    val sb = Module(new SB(4))
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
    sb_cmt_hazard       := sb.io.wb_valid && inst_rf3.mem_type(5)
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

    val iq_self_wake_preg = VecInit(Mux(!mdu.io.busy, md_ex2_ex3_reg.io.inst_pack_EX2.prd, 0.U),sel1.io.wake_preg, sel2.io.wake_preg, re_reg3.io.inst_pack_EX.prd)
    val iq_mutual_wake_preg = VecInit(Mux(!mdu.io.busy, md_ex2_ex3_reg.io.inst_pack_EX2.prd, 0.U),ir_reg1.io.inst_pack_RF.prd,ir_reg2.io.inst_pack_RF.prd,Mux(!dcache.io.cache_miss_MEM(4), re_reg3.io.inst_pack_EX.prd, 0.U))

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
    val imm_rf3 = reg1(Mux(inst_iq3.priv_vec(10), Fill(5, inst_iq3.imm(31)) ## inst_iq3.inst_pack_RF.imm(31, 5), inst_iq3.inst_pack_RF.imm), ir3_stall, predict_fail)
    val prj_data_rf3 = reg_fw(rf.io.prj_data(3),
        bypass.io.forward_prj_en(3), bypass.io.forward_prj_data(3),
        ir3_stall, predict_fail)
    val prk_data_rf3 = reg_fw(rf.io.prk_data(3),
        bypass.io.forward_prk_en(3), bypass.io.forward_prk_data(3),
        ir3_stall, predict_fail)
    
    // regfile read -------------------------
    rf.io.prj := VecInit(inst_rf0.prj, inst_rf1.prj, inst_rf2.prj, inst_iq3.prj)
    rf.io.prk := VecInit(inst_rf0.prk, inst_rf1.prk, inst_rf2.prk, inst_iq3.prk)
    //todo: CSR read for MD pipeline (0)
    //todo: RF stage for LS pipeline (3)
    // RF-EX
    val inst_ex0 = reg1(inst_rf0, mdu.io.busy, predict_fail)
    val inst_ex1 = reg1(inst_rf1, false.B, predict_fail)
    val inst_ex2 = reg1(inst_rf2, false.B, predict_fail)
    val re3_stall = dcache_miss_hazard || sb_full_hazard 
    val re3_flush = predict_fail || !re3_stall && sb_cmt_hazard
    val inst_ex3 = reg1(inst_rf3, re3_stall, re3_flush)

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
    val prj_data_ex3 = reg1(prj_data_rf3 + imm_rf3, re3_stall, re3_flush)
    val prk_data_ex3 = reg1(prk_data_rf3, re3_stall, re3_flush)

    // val prj_data_ex3 = 
    // execute -------------------------
    // alu1.io.alu_op := 
    //LS execute

    //rob LS
    rob.io.ex.priv_vec_ls := inst_ex3.priv_vec(12,10)

    //exception ALE dectect
    val addr_mask = (1.U(4.W) << inst_ex3.mem_type(1, 0)) - 1.U
    val Is_ALE = Wire(UInt(8.W))
    Is_ALE := Mux((prj_data_ex3 & addr_mask) =/= 0.U, 1.U(1.W) ## 0x09.U(7.W), 0.U(8.W))
    val exception_ls = Mux((inst_ex3.priv_vec(10) && inst_ex3.imm(4, 3) =/= 2.U || inst_ex3.priv_vec(12) && !csr.io.llbit_global), 0.U, Is_ALE)

    //EX-MEM Reg
    val em_stall = dcache_miss_hazard
    val em_flush = predict_fail || (!em_stall && sb_full_hazard)
    val inst_mem = reg1(inst_ex3, em_stall, em_flush)
    val prj_data_mem = reg1(prj_data_ex3, em_stall, em_flush)
    val prk_data_mem = reg1(prk_data_ex3, em_stall, em_flush)
    val llbit_mem = reg1(csr.io.llbit_global, em_stall, em_flush)
    val exception_mem = reg1(exception_ls, em_stall, em_flush)

    //store buffer
    sb.io.flush := predict_fail
    sb.io.addr_ex := prj_data_ex3
    sb.io.st_data_ex := prk_data_ex3
    sb.io.mem_type_ex := Mux(re3_stall, 0.U, inst_ex3.mem_type)
    sb.io.uncache_ex := false.B
    sb.io.st_num := rob.io.store_num_cmt
    sb.io.dcache_miss := dcache_miss_hazard
    sb.io.em_stall := em_stall

    //dcache
    dcache.io.addr_EX := Mux(sb.io.wb_valid, sb.io.addr_out, prj_data_rf3 + imm_rf3)
    dcache.io.wdata_EX := Mux(sb.io.wb_valid, sb.io.addr_out, prk_data_rf3)
    dcache.io.mem_type_EX := Mux(sb.io.wb_valid, Mux(sb.io.uncache_out, 0.U, sb.io.mem_type_out), Mux(sb_full_hazard, 0.U, inst_rf3.mem_type))
    dcache.io.store_cmt_EX := sb.io.wb_valid
    dcache.io.cacop_en := Mux(sb.io.wb_valid, false.B, inst_rf3.priv_vec(10) && inst_rf3.imm(2,0) === 1.U)
    dcache.io.cacop_op := inst_rf3.imm(4,3)
    dcache.io.uncache := false.B
    dcache.io.rob_index_TC := inst_ex3.rob_index
    dcache.io.paddr_TC := prj_data_ex3
    dcache.io.exception := exception_mem
    dcache.io.rob_index_CMT := rob.io.rob_index_cmt
    dcache.io.d_rready := arb.io.d_rready
    dcache.io.d_rlast := arb.io.rlast
    dcache.io.d_rdata := arb.io.rdata
    dcache.io.d_wready := arb.io.wready
    dcache.io.d_bvalid := arb.io.bvalid
    dcache.io.stall := false.B
    dcache.io.flush := predict_fail

    val mem_rdata_raw = VecInit.tabulate(4)(i => Mux(sb.io.ld_hit(i), sb.io.ld_data_mem(i*8+7, i*8), dcache.io.rdata_MEM(i*8+7, i*8))).asUInt
    val mem_rdata = MuxLookup(inst_mem.mem_type(3, 0), 0.U)(Seq(
                                                        0.U -> Fill(24, mem_rdata_raw(7)) ## mem_rdata_raw(7, 0),
                                                        1.U -> Fill(16, mem_rdata_raw(15)) ## mem_rdata_raw(15, 0),
                                                        2.U -> mem_rdata_raw,
                                                        8.U -> 0.U(24.W) ## mem_rdata_raw(7, 0),
                                                        9.U -> 0.U(16.W) ## mem_rdata_raw(15, 0)))
    val ls_wb_data = Mux(inst_mem.priv_vec(12), 0.U(31.W) ## llbit_mem, mem_rdata)

    //WB reg
    val wb3_stall = false.B
    val wb3_flush = predict_fail || dcache_miss_hazard

    val inst_wb3 = reg1(inst_mem, wb3_stall, wb3_flush)
    val exception_wb3 = reg1(exception_mem, wb3_stall, wb3_flush)
    val vaddr_wb3 = reg1(prj_data_mem, wb3_stall, wb3_flush)
    val mem_rdata_wb3 = reg1(ls_wb_data, wb3_stall, wb3_flush)

    // arbiter
    arb.io.i_araddr                 := icache.io.i_araddr
    arb.io.i_rvalid                 := icache.io.i_rvalid
    arb.io.i_rsize                  := icache.io.i_rsize
    arb.io.i_rburst                 := icache.io.i_rburst
    arb.io.i_rlen                   := icache.io.i_rlen

    //arb.io.d_araddr                 := dcache.io.d_araddr
    arb.io.d_rvalid                 := dcache.io.d_rvalid
    arb.io.d_rsize                  := dcache.io.d_rsize
    arb.io.d_rburst                 := dcache.io.d_rburst
    arb.io.d_rlen                   := dcache.io.d_rlen
    //arb.io.d_awaddr                 := dcache.io.d_awaddr
    arb.io.d_wvalid                 := dcache.io.d_wvalid
    arb.io.d_wdata                  := dcache.io.d_wdata
    arb.io.d_wlast                  := dcache.io.d_wlast
    arb.io.d_wsize                  := dcache.io.d_wsize
    arb.io.d_wburst                 := dcache.io.d_wburst
    arb.io.d_wlen                   := dcache.io.d_wlen
    //arb.io.d_wstrb                  := dcache.io.d_wstrb
    arb.io.d_bready                 := dcache.io.d_bready


    io.araddr                       := arb.io.araddr
    io.arburst                      := arb.io.arburst
    io.arid                         := arb.io.arid
    io.arlen                        := arb.io.arlen
    arb.io.arready                  := io.arready
    io.arsize                       := arb.io.arsize
    io.arvalid                      := arb.io.arvalid

    io.awaddr                       := arb.io.awaddr
    io.awburst                      := arb.io.awburst
    io.awid                         := arb.io.awid
    io.awlen                        := arb.io.awlen
    arb.io.awready                  := io.awready
    io.awsize                       := arb.io.awsize
    io.awvalid                      := arb.io.awvalid

    arb.io.bid                      := io.bid
    io.bready                       := arb.io.bready
    arb.io.bresp                    := io.bresp
    arb.io.bvalid                   := io.bvalid

    arb.io.rdata                    := io.rdata
    arb.io.rid                      := io.rid
    arb.io.rlast                    := io.rlast
    io.rready                       := arb.io.rready
    arb.io.rresp                    := io.rresp
    arb.io.rvalid                   := io.rvalid

    io.wdata                        := arb.io.wdata
    io.wlast                        := arb.io.wlast
    arb.io.wready                   := io.wready
    io.wstrb                        := arb.io.wstrb
    io.wvalid                       := arb.io.wvalid
}
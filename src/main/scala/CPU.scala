import chisel3._
import chisel3.util._
import Configs._
import Interfaces._
import InstPacks._
import Util._

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
    val decode = Module(new Decoder)
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
    // ======================================
    // pre fetch
    pc.io.stall         := false.B
    pc.io.pred_jump     := predict.io.pred_jump
    pc.io.pred_npc      := predict.io.pred_npc
    pc.io.predict_fail  := false.B
    pc.io.branch_target := 0.U
    pc.io.pd_fix_en     := pd.io.pd_fix_en
    pc.io.pd_fix_target := pd.io.pd_fix_target
    pc.io.idle_start    := false.B
    pc.io.idle_intr     := false.B

    predict.io.pc       := pc.io.pc
    predict.io.npc      := pc.io.npc

    val insts_PF = VecInit.tabulate(2)(i => {
        val inst = Wire(new pack_PF)
        inst.inst_valid := pc.io.inst_valid(i)
        inst.pc         := pc.io.pc
        inst.pred_valid := predict.io.pred_valid
        inst.pred_jump  := predict.io.pred_jump
        inst.pred_npc   := predict.io.pred_npc
        inst.br_cnt     := predict.io.br_cnt(i)
        inst.exception  := pc.io.exception
        inst
    })

    // PF-IF
    val insts_IF = reg1(insts_PF, false.B, false.B)
    // instruction fetch
    
}
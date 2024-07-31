// package ROB

import chisel3._
import chisel3.util._
import java.{util => ju}
import Configs._
import Interfaces._
import Util._

// ROB表项 见P323
class ROB_Item() extends Bundle{
    val exception     = UInt(8.W)
    val rd            = UInt(5.W)              // 目的逻辑寄存器 (rd)
    val rd_valid      = Bool()                 // 是否写
    val prd           = UInt(PREG_W.W)         // 物理寄存器
    val pprd          = UInt(PREG_W.W)         // 原物理寄存器
    val pc            = UInt(32.W)
    val is_store      = Bool()
    val is_br         = Bool()
    val br_type       = UInt(2.W)              // 用于分支预测
    val is_priv_wrt   = Bool()
    val is_priv_ls    = Bool()
    val inst          = UInt(32.W)             // debug
    val predict_fail  = Bool()
    val real_jump     = Bool()
    val branch_target = UInt(32.W)
    val br_cnt        = UInt(2.W)
    val rf_wdata      = UInt(32.W)             // debug
    val complete      = Bool()
}

class Priv_Buf extends Bundle{
    val valid       = Bool()
    val priv_vec    = UInt(10.W)
    val csr_addr    = UInt(14.W)
    val tlb_entry   = new TLB_Entry
    val inv_op      = UInt(5.W)
    val inv_vaddr   = UInt(32.W)
    val inv_asid    = UInt(10.W)
}
class Priv_LS_Buf extends Bundle{
    val valid       = Bool()
    val priv_vec    = UInt(3.W)
}

class ROB() extends Module{
    val io = IO(new Bundle{
        //约定：发不出2条指令就全别发，dp_valid = 0
        val dp_valid        = Input(Bool())                    // dp 有效
        val dp              = Input(Vec(2, new DP_to_ROB))
        val rob_index       = Output(Vec(2, UInt(ROB_W.W)))
        val wb              = Input(Vec(4, new WB_to_ROB))
        val arat            = Output(Vec(2, new ROB_to_ARAT))
        val predict_fail    = Input(Bool())
        val full            = Output(Bool())
        val stall           = Input(Bool())
        // priv
        val ex = Input(new Bundle{
            val priv_vec       = Input(UInt(10.W))
            val csr_addr       = Input(UInt(14.W))
            val tlb_entry      = Input(new TLB_Entry)
            val invtlb_op      = Input(UInt(5.W))
            val invtlb_vaddr   = Input(UInt(32.W))
            val invtlb_asid    = Input(UInt(10.W))
            val priv_vec_ls    = Input(UInt(3.W))
        })
        // from CSR
        val eentry            = Input(UInt(32.W))
        val tlbrentry         = Input(UInt(32.W))
        val llbit             = Input(Bool())

        val predict_fail_cmt  = Output(Bool())
        val branch_target_cmt = Output(UInt(32.W))
        val exception_cmt     = Output(UInt(8.W))

        // 更新 predict
        val pred = Output(new Bundle{
            val update_en     = Output(Bool())
            val branch_target = Output(UInt(32.W))
            val pc_cmt        = Output(UInt(32.W))
            val real_jump     = Output(Bool())
            val br_type       = Output(UInt(2.W))
            val br_cnt        = Output(UInt(2.W))
        })
        // 更新 store buffer
        val store_num_cmt     = Output(UInt(2.W))
        // 执行特权指令/异常
        val csr_addr_cmt      = Output(UInt(14.W))
        val csr_wdata_cmt     = Output(UInt(32.W))
        val csr_we_cmt        = Output(Bool())
        val tlb_entry_cmt     = Output(new TLB_Entry)
        val badv_cmt          = Output(UInt(32.W))
        val is_ertn_cmt       = Output(Bool())
        val idle_en_cmt       = Output(Bool())
        val llbit_set_cmt     = Output(Bool())
        val llbit_clear_cmt   = Output(Bool())
        
        val rob_index_cmt     = Output(UInt(PREG_W.W))
    })

    //rob表 2*(ROB_SIZE/2)
    val rob = RegInit(
        VecInit.fill(2)(
            VecInit.fill(ROB_SIZE / 2)(
                0.U.asTypeOf(new ROB_Item))))
    val priv_buf    = RegInit(0.U.asTypeOf(new Priv_Buf))
    val priv_ls_buf = RegInit(0.U.asTypeOf(new Priv_LS_Buf))
                
    val head = RegInit(0.U(ROB_W.W)) //最旧
    val tail = RegInit(0.U((ROB_W - 1).W)) //最新
    //tail必定每次进来一行(2个)，head不一定
    
    // dp: 接收dispatch的输入，填入新表项，tail++，返回rob_index
    when(io.dp_valid && !io.full) {
        for(i <- 0 until 2) {
            rob(i)(tail).exception   := io.dp(i).exception
            rob(i)(tail).rd          := io.dp(i).rd
            rob(i)(tail).rd_valid    := io.dp(i).rd_valid
            rob(i)(tail).prd         := io.dp(i).prd
            rob(i)(tail).pprd        := io.dp(i).pprd
            rob(i)(tail).pc          := io.dp(i).pc
            rob(i)(tail).is_store    := io.dp(i).is_store
            rob(i)(tail).is_br       := io.dp(i).is_br
            rob(i)(tail).br_type     := io.dp(i).br_type
            rob(i)(tail).br_cnt      := io.dp(i).br_cnt
            rob(i)(tail).is_priv_wrt := io.dp(i).priv_vec(9, 1).orR
            rob(i)(tail).is_priv_ls  := io.dp(i).priv_vec(12, 10).orR
            rob(i)(tail).inst        := io.dp(i).inst
            rob(i)(tail).complete    := false.B
            tail := tail + 1.U
        }
    }
    io.rob_index := VecInit.tabulate(2)(i => Cat(tail, i.U))
    io.full := (tail + 1.U === head)

    // wb: 接收4个FU的结果，填入表项
    for(i <- 0 until 4) {
        when(io.wb(i).valid) {
            val col = io.wb(i).rob_index(0)
            val row = io.wb(i).rob_index(ROB_W - 1, 1)
            val item = rob(col)(row)
            item.complete := true.B
            item.rf_wdata := io.wb(i).rf_wdata
            if (i == 0){ // MD
                //ERTN
                item.branch_target := Mux(item.exception(7), item.pc + 4.U, io.wb(i).branch_target)
            }
            if (i == 2){ // BR
                item.predict_fail  := io.wb(i).predict_fail
                item.real_jump     := io.wb(i).real_jump
                item.branch_target := io.wb(i).branch_target
            }
            if (i == 3){ // LS
                item.exception     := io.wb(i).exception
                item.branch_target := io.wb(i).branch_target + 4.U
            }
        }
    }
    // ex: 接收priv信息存入priv_buf
    when(!priv_buf.valid && io.ex.priv_vec(9, 1).orR){
        priv_buf.csr_addr  := io.ex.csr_addr
        priv_buf.priv_vec  := io.ex.priv_vec
        priv_buf.tlb_entry := io.ex.tlb_entry
        priv_buf.inv_op    := io.ex.invtlb_op
        priv_buf.inv_vaddr := io.ex.invtlb_vaddr
        priv_buf.inv_asid  := io.ex.invtlb_asid
        priv_buf.valid     := true.B
    }
    when(!priv_ls_buf.valid && io.ex.priv_vec_ls.orR){
        priv_ls_buf.priv_vec := io.ex.priv_vec_ls
        priv_ls_buf.valid    := true.B
    }
    when(io.predict_fail_cmt){
        priv_buf.valid    := false.B
        priv_ls_buf.valid := false.B
    }
    // commit: 将完成的指令退休
    // TODO: empty处理 
    val commit_en  = Wire(Vec(2, Bool())) 
    val col = Wire(Vec(2, Bool()))
    val row = Wire(Vec(2, UInt((ROB_W - 1).W)))
    col(0) := head(0)
    col(1) := head(0) ^ 1.U
    row(0) := head(ROB_W - 1, 1)
    row(1) := wrap(head + 1.U, ROB_SIZE.U)(ROB_W - 1, 1)
    val empty = VecInit.tabulate(2)(i => row(i) === tail)
    val commit_item = VecInit.tabulate(2)(i => rob(col(i))(row(i)))
    commit_en(0) := !empty(0) && commit_item(0).complete
    commit_en(1) := !empty(1) && commit_item(1).complete && commit_en(0) && 
        !(commit_item(0).is_br || commit_item(0).exception(7) || commit_item(0).is_priv_ls || commit_item(0).is_priv_wrt)
    val commit_num = PopCount(commit_en)

    //将管理寄存器的操作交给ARAT
    for(i <- 0 until 2) {
        val arat = Wire(new ROB_to_ARAT)
        arat.commit_en := commit_en(i)
        arat.rd_valid  := commit_item(i).rd_valid
        arat.prd       := commit_item(i).prd
        arat.pprd      := commit_item(i).pprd
        io.arat(i) := reg1(arat)
    }
    head := wrap(head +& PopCount(commit_en), ROB_SIZE.U)
    
    val update_item = Mux(commit_en(0), Mux(commit_en(1), commit_item(1), commit_item(0)), 0.U.asTypeOf(new ROB_Item))
    
    val predict_fail_cmt = update_item.predict_fail || update_item.is_priv_wrt || update_item.is_priv_ls || update_item.exception(7);
    val branch_target_cmt = Mux(update_item.exception(7),
        Mux(update_item.exception(5, 0) === 0x3f.U, io.tlbrentry, io.eentry),
        Mux(update_item.is_priv_wrt && priv_buf.priv_vec(3) //ERTN
            || update_item.is_br && update_item.real_jump,
            update_item.branch_target, update_item.pc + 4.U))
    io.predict_fail_cmt  := reg1(predict_fail_cmt)
    io.branch_target_cmt := reg1(branch_target_cmt)
    io.exception_cmt     := reg1(update_item.exception)
    when(predict_fail_cmt || io.predict_fail_cmt){
        head := 0.U
        tail := 0.U
    }

    // 更新predict
    io.pred.update_en       := reg1(update_item.is_br)
    io.pred.branch_target   := reg1(update_item.branch_target)
    io.pred.pc_cmt          := reg1(update_item.pc)
    io.pred.real_jump       := reg1(update_item.real_jump)
    io.pred.br_type         := reg1(update_item.br_type)
    io.pred.br_cnt          := reg1(update_item.br_cnt)

    // 更新store buffer
    val store_num_cmt = PopCount(VecInit.tabulate(2)(i => 
        commit_item(i).is_store && commit_en(i) && !commit_item(i).exception(7)
        && !(commit_item(i).is_priv_ls && !reg1(io.llbit))))
    io.store_num_cmt := reg1(store_num_cmt)
    
    // 更新csr
    io.csr_we_cmt    := reg1(update_item.is_priv_wrt && priv_buf.priv_vec(2, 1).orR)
    io.csr_addr_cmt  := reg1(priv_buf.csr_addr)
    io.csr_wdata_cmt := reg1(update_item.branch_target) // 存放MD的csr_wdata
    io.tlb_entry_cmt := reg1(priv_buf.tlb_entry)
    io.badv_cmt      := reg1(update_item.branch_target) // 存放LS的vaddr
    io.is_ertn_cmt   := reg1(update_item.is_priv_wrt && priv_buf.priv_vec(3))
    io.idle_en_cmt   := reg1(update_item.is_priv_wrt && priv_buf.priv_vec(9))
    io.llbit_set_cmt   := reg1(update_item.is_priv_ls && priv_ls_buf.priv_vec(1))
    io.llbit_clear_cmt := reg1(update_item.is_priv_ls && priv_ls_buf.priv_vec(2))

    io.rob_index_cmt := reg1(head)
} 
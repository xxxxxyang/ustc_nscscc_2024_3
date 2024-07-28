import chisel3._
import chisel3.util._
import Predict_Config._
import Predict_Struct._

object Predict_Struct{
    import Predict_Config._
    class btb_t extends Bundle{
        val valid       = Bool()
        val target      = UInt(30.W)
        val tag         = UInt(BTB_TAG_WIDTH.W)
        val typ         = UInt(2.W)
    }
}

class Predict_IO extends Bundle{
    val pc                  = Input(UInt(32.W))
    val npc                 = Input(UInt(32.W))
    val pred_jump           = Output(Vec(2, Bool()))
    val pred_valid          = Output(Vec(2, Bool()))
    val pred_npc            = Output(UInt(32.W))

    // commit info from ROB
    val pc_cmt              = Input(UInt(32.W))  // 一周期内最多提交一条分支指令
    val real_jump           = Input(Bool())
    val branch_target       = Input(UInt(32.W))
    val update_en           = Input(Bool())
    val br_type             = Input(UInt(2.W))

    // recover info 
    val top_arch            = Input(UInt(3.W))
    val ras_arch            = Input(Vec(8, UInt(32.W)))
    val predict_fail        = Input(Bool())
    val ghr_arch            = Input(UInt(GHR_WIDTH.W))

    // from pre-decode
    val pd_fix_en           = Input(Bool())
    val pd_fix_is_bl        = Input(Bool())
    val pd_fix_pc           = Input(UInt(32.W))

}
class Predict extends Module {
    val io = IO(new Predict_IO)

    val pc = io.pc
    val npc = io.npc
    val pc_cmt = io.pc
    val pc_col = pc_cmt(2)   
    val pred_valid = Wire(Vec(2, Bool()))
    val pred_jump = Wire(Vec(2, Bool()))
    val update_en = io.update_en

    // GHR 全局历史寄存器
    val ghr = RegInit(0.U(GHR_WIDTH.W))
    val sel_pred = pred_valid.asUInt.orR && !io.pd_fix_en
    val jump = (pred_valid.asUInt & pred_jump.asUInt).orR
    when(io.predict_fail){
        ghr := io.ghr_arch
    }.elsewhen(sel_pred){
        ghr := ghr(GHR_WIDTH - 2, 0) ## jump
    }

    val pht_rindex = (ghr ^ pc(PHT_INDEX_WIDTH + 2, PHT_INDEX_WIDTH - GHR_WIDTH + 3)) ## pc(PHT_INDEX_WIDTH - GHR_WIDTH + 2, 3)

    // PHT 模式历史表
    val pht = RegInit(VecInit.fill(2)(VecInit.fill(PHT_DEPTH)(2.U(2.W))))
    val pht_rdata = VecInit.tabulate(2)(i => pht(i)(pht_rindex))
    val pht_windex = (ghr ^ pc_cmt(PHT_INDEX_WIDTH + 2, PHT_INDEX_WIDTH - GHR_WIDTH + 3)) ## pc_cmt(PHT_INDEX_WIDTH - GHR_WIDTH + 2, 3)
    val pht_raw_rdata = pht(pc_col)(pht_windex)

    when(update_en){
        pht(pc_col)(pht_windex) := Mux(io.real_jump, 
                pht_raw_rdata + (pht_raw_rdata =/= 3.U), pht_raw_rdata - (pht_raw_rdata =/= 0.U))
    }
    
    // BTB 分支目标缓存
    val btb_tag     = VecInit.fill(2)(Module(new xilinx_simple_dual_port_1_clock_ram_read_first(BTB_TAG_WIDTH+1, BTB_DEPTH)).io)
    val btb_trgt    = VecInit.fill(2)(Module(new xilinx_simple_dual_port_1_clock_ram_read_first(30+2, BTB_DEPTH)).io)
    val btb_rindex  = VecInit.tabulate(2)(i => npc(3+BTB_INDEX_WIDTH-1, 3))
    val btb_rdata   = Wire(Vec(2, new btb_t))
    val btb_windex  = pc_cmt(3-1+BTB_INDEX_WIDTH, 3)
    
    for(i <- 0 until 2){
        btb_tag(i).addra    := btb_windex
        btb_tag(i).addrb    := btb_rindex(i)
        btb_tag(i).dina     := true.B ## pc_cmt(31, 32 - BTB_TAG_WIDTH)
        btb_tag(i).clka     := clock
        btb_tag(i).wea      := update_en && (i.U === pc_col)        
        btb_trgt(i).addra   := btb_windex
        btb_trgt(i).addrb   := btb_rindex(i)
        btb_trgt(i).dina    := io.branch_target(31, 2) ## io.br_type
        btb_trgt(i).clka    := clock
        btb_trgt(i).wea     := update_en && (i.U === pc_col)
        btb_rdata(i).target := btb_trgt(i).doutb(31, 2)
        btb_rdata(i).typ    := btb_trgt(i).doutb(1, 0)  // data最低两位存储分支类型
        btb_rdata(i).valid  := btb_tag(i).doutb(BTB_TAG_WIDTH)  // TAG最高位存储valid
        btb_rdata(i).tag    := btb_tag(i).doutb(BTB_TAG_WIDTH - 1, 0)
    }

    val valid_mask      = true.B ## !pc(2)  // 如果pc不是8对齐，则只有前一条有效
    pred_valid          := VecInit.tabulate(2)(
        i => btb_rdata(i).valid && !(btb_rdata(i).tag ^ pc(31, 32 - BTB_TAG_WIDTH))
                                && valid_mask(i)
    )
    pred_jump           := VecInit.tabulate(2)(
        i => btb_rdata(i).valid && !(btb_rdata(i).tag ^ pc(31, 32 - BTB_TAG_WIDTH))
                                && valid_mask(i) && pht_rdata(i)(1)
    )
    val pred_hit_index  = !pred_jump(0)

    // RAS
    val ras         = RegInit(VecInit.fill(8)(0x1c000000.U(32.W)))
    val top         = RegInit(0x7.U(3.W))

    when(io.predict_fail){
        top             := io.top_arch
        ras             := io.ras_arch
    }.elsewhen(io.pd_fix_en && io.pd_fix_is_bl){
        top             := top + 1.U
        ras(top + 1.U)  := io.pd_fix_pc
    }.elsewhen(btb_rdata(pred_hit_index).typ(1) && pred_valid(pred_hit_index)){
        top             := top + 1.U
        ras(top + 1.U)  := pc(31, 3) ## pred_hit_index ## 0.U(2.W)
    }.elsewhen(btb_rdata(pred_hit_index).typ === RET && pred_valid(pred_hit_index)){
        top             := top - 1.U
    }

    // result
    io.pred_jump        := (pred_jump(1) ## Mux(pc(2), pred_jump(1), pred_jump(0))).asBools
    io.pred_valid       := (pred_valid(1) ## Mux(pc(2), pred_valid(1), pred_valid(0))).asBools
    io.pred_npc         := Mux(btb_rdata(pred_hit_index).typ === RET, 
                            ras(top-1.U) + 4.U, btb_rdata(pred_hit_index).target ## 0.U(2.W))
}


// 后续优化方向：
// 1. RAS增加递归、溢出计数器
// 2. 修改BTB和RAS的更新逻辑
// 3. BTB增加Agree机制（单缺失问题待解决）
// 4. 使用多级BTB，参考《高性能》一书
// 5. 使用分页索引BTB，参考Rocket处理器
// 6. 使用自校正RAS，参考《高性能》提到的论文
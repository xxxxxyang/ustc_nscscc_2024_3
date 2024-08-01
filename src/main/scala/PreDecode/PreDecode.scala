import chisel3._
import chisel3.util._
import InstPacks._
import control_signal._

class PreDecode extends Module {
    val io = IO(new Bundle{
        val insts           = Input(Vec(2, new pack_IF))
        // for fetch queue
        val insts_PD        = Output(Vec(2, new pack_PD))

        val pd_fix_en       = Output(Bool())
        val pd_fix_target   = Output(UInt(32.W))
        val pd_fix_is_bl    = Output(Bool())       // for ras
        val pd_fix_pc       = Output(UInt(32.W))   // for ras
    })
    // generate jump_type
    val j_JIRL :: j_NOT :: j_ALWAYS :: j_COND :: Nil = Enum(4)
    val inst            = VecInit(io.insts.map(_.inst))
    val jump_type       = VecInit.tabulate(2)(i => 
        Mux(inst(i)(31, 30) =/= "b01".U, j_NOT,
        Mux(inst(i)(29, 26) === BR_JIRL, j_JIRL,
        Mux(inst(i)(29, 26) === BR_B || inst(i)(29, 26) === BR_BL, j_ALWAYS, j_COND))))
    
    // generate insts_PD and fix
    val insts_PD  = WireDefault(io.insts)
    val need_fix  = Wire(Vec(2, Bool()))

    for(i <- 0 until 2){
        need_fix(i) := false.B
        when(io.insts(i).inst_valid){
            switch(jump_type(i)){
                is(j_NOT){
                    need_fix(i) := io.insts(i).pred_jump
                    insts_PD(i).pred_jump := false.B
                    insts_PD(i).pred_npc  := io.insts(i).pc + 4.U
                }
                is(j_ALWAYS){ //B, BL
                    val npc = io.insts(i).pc + (Fill(4, inst(i)(9)) ## inst(i)(9, 0) ## inst(i)(25, 10) ## 0.U(2.W))
                    need_fix(i) := !io.insts(i).pred_jump || io.insts(i).pred_npc =/= npc
                    insts_PD(i).pred_jump := true.B
                    insts_PD(i).pred_npc  := npc
                }
                is(j_COND){
                    val npc = io.insts(i).pc + (Fill(14, inst(i)(25)) ## inst(i)(25, 10) ## 0.U(2.W))
                    when(!io.insts(i).pred_valid){
                        val is_back = inst(i)(25)
                        need_fix(i)           := is_back
                        insts_PD(i).pred_jump := is_back
                        insts_PD(i).pred_npc  := Mux(is_back, npc, io.insts(i).pc + 4.U)
                    }
                }
            }
        }
    }
    insts_PD(0).inst_valid      := io.insts(0).inst_valid
    insts_PD(1).inst_valid      := io.insts(0).inst_valid && io.insts(1).inst_valid && !need_fix(0)
    
    // 前端异常处理
    for(i <- 0 until 2){
        when(io.insts(i).exception.orR){
            insts_PD(i).inst := 0x00100000.U //ADD r0, r0, r0
        }
    }

    io.insts_PD        := insts_PD

    val fix_index = !need_fix(0)
    io.pd_fix_en       := need_fix.asUInt.orR
    io.pd_fix_target   := insts_PD(fix_index).pred_npc
    io.pd_fix_is_bl    := inst(fix_index)(29, 26) === BR_BL
    io.pd_fix_pc       := io.insts(fix_index).pc
}

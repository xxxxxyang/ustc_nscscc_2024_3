import chisel3._
import chisel3.util._
import TLB_Config._
import TLB_Struct._

class MMU_IO extends Bundle {
    // from csr
    val csr_asid        = Input(UInt(10.W))  
    val csr_plv         = Input(UInt(2.W))
    val csr_tlbehi      = Input(UInt(19.W))
    val csr_tlbidx      = Input(UInt(TLB_IDX_WID.W))
    val csr_dmw0        = Input(UInt(32.W))
    val csr_dmw1        = Input(UInt(32.W))
    val csr_crmd_trans  = Input(UInt(6.W))

    // for icache 
    val i_valid         = Input(Bool())
    val i_vaddr         = Input(UInt(32.W))
    val i_stall         = Input(Bool())
    val i_paddr         = Output(UInt(32.W))
    val i_uncache       = Output(Bool())
    val i_exception     = Output(UInt(8.W))
    
    // for dcache 
    val d_rvalid        = Input(Bool())
    val d_wvalid        = Input(Bool())
    val d_vaddr         = Input(UInt(32.W))
    val d_stall         = Input(Bool())
    val d_paddr         = Output(UInt(32.W))
    val d_uncache       = Output(Bool())
    val d_exception     = Output(UInt(8.W))

    // tlbsrch
    val tlbsrch_idx     = Output(UInt(TLB_IDX_WID.W))
    val tlbsrch_hit     = Output(Bool())
    
    // tlbrd 
    val tlbrd_entry    = Output(new tlb_t)

    // tlbwr
    val tlbwr_entry    = Input(new tlb_t)
    val tlbwr_en       = Input(Bool())
    
    // tlbfill
    val tlbfill_idx    = Input(UInt(TLB_IDX_WID.W))
    val tlbfill_en     = Input(Bool())

    // invtlb
    val invtlb_en      = Input(Bool())
    val invtlb_op      = Input(UInt(5.W))
    val invtlb_asid    = Input(UInt(10.W))
    val invtlb_vaddr   = Input(UInt(32.W))
}

class MMU extends Module{
    val io = IO(new MMU_IO)
    val tlb = Module(new TLB)

    io <> tlb.io

    // for icache
    val dmw0_reg          = RegNext(io.csr_dmw0)
    val dmw1_reg          = RegNext(io.csr_dmw1)
    val i_vaddr           = io.i_vaddr
    val csr_crmd_trans    = io.csr_crmd_trans
    val i_dmw0_hit = (!(i_vaddr(31, 29) ^ dmw0_reg(31, 29))) && dmw0_reg(3, 0)(io.csr_plv)
    val i_dmw1_hit = (!(i_vaddr(31, 29) ^ dmw1_reg(31, 29))) && dmw1_reg(3, 0)(io.csr_plv)
    val i_uncache_direct  = (i_vaddr(31,16) === "hbfaf".U) || (i_vaddr(31,16) === "h1faf".U)
    // val i_uncache_direct = !csr_crmd_trans(2)
    val i_uncache         = Mux(csr_crmd_trans(0), i_uncache_direct, 
                            Mux(i_dmw0_hit, !dmw0_reg(4),
                            Mux(i_dmw1_hit, !dmw1_reg(4), tlb.io.i_tlb_uncache)))
    val i_paddr           = Mux(csr_crmd_trans(0), i_vaddr, 
                            Mux(i_dmw0_hit, dmw0_reg(27, 25) ## i_vaddr(28, 0), 
                            Mux(i_dmw1_hit, dmw1_reg(27, 25) ## i_vaddr(28, 0), tlb.io.i_tlb_paddr)))
    val i_exception_ne    = ShiftRegister(csr_crmd_trans(0) || i_dmw0_hit || i_dmw1_hit, 1, !io.i_stall)
    io.i_exception       := Mux(i_exception_ne, 0.U, tlb.io.i_tlb_exception)

    // for dcache
    val plv_reg           = RegNext(io.csr_plv)
    val da_reg            = RegNext(csr_crmd_trans(0))
    val d_vaddr           = io.d_vaddr
    val d_uncache_direct  = (d_vaddr(31,16) === "hbfaf".U) || (d_vaddr(31,16) === "h1faf".U)
    // val d_uncache_direct = !csr_crmd_trans(4)
    val d_dmw0_hit  = (!(d_vaddr(31, 29) ^ dmw0_reg(31, 29))) && dmw0_reg(3, 0)(plv_reg)
    val d_dmw1_hit  = (!(d_vaddr(31, 29) ^ dmw1_reg(31, 29))) && dmw1_reg(3, 0)(plv_reg)
    val d_paddr           = Mux(da_reg, d_vaddr,
                            Mux(d_dmw0_hit, dmw0_reg(27, 25) ## d_vaddr(28, 0), 
                            Mux(d_dmw1_hit, dmw1_reg(27, 25) ## d_vaddr(28, 0), tlb.io.d_tlb_paddr)))
    val d_uncache         = Mux(da_reg, d_uncache_direct, 
                            Mux(d_dmw0_hit, !dmw0_reg(4),
                            Mux(d_dmw1_hit, !dmw1_reg(4), tlb.io.d_tlb_uncache)))
    val d_exception_ne    = da_reg || d_dmw0_hit || d_dmw1_hit
    io.d_exception       := Mux(d_exception_ne, 0.U, tlb.io.d_tlb_exception)
}
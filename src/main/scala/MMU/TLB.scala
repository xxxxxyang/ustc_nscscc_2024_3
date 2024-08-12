import chisel3._
import chisel3.util._
import exception_code._
import TLB_Config._
import TLB_Struct._
import Util.reg1

class TLB_IO extends Bundle {
    // from csr
    val csr_asid        = Input(UInt(10.W))  
    val csr_plv         = Input(UInt(2.W))
    val csr_tlbehi      = Input(UInt(19.W))
    val csr_tlbidx      = Input(UInt(TLB_IDX_WID.W))

    // for icache 
    val i_valid         = Input(Bool())
    val i_vaddr         = Input(UInt(32.W))
    val i_stall         = Input(Bool())
    val i_tlb_paddr     = Output(UInt(32.W))
    val i_tlb_uncache   = Output(Bool())
    val i_tlb_exception = Output(UInt(8.W))
    
    // for dcache 
    val d_rvalid        = Input(Bool())
    val d_wvalid        = Input(Bool())
    val d_vaddr         = Input(UInt(32.W))
    val d_stall         = Input(Bool())
    val d_tlb_paddr     = Output(UInt(32.W))
    val d_tlb_uncache   = Output(Bool())
    val d_tlb_exception = Output(UInt(8.W))

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

class TLB extends Module{
    val io = IO(new TLB_IO)

    // 为优化时序，构建两个tlb以减少端口，内容只差一个周期
    val i_tlb = RegInit(VecInit.fill(TLB_ENTRY_NUM)(0.U.asTypeOf(new tlb_t)))
    val d_tlb = RegInit(VecInit.fill(TLB_ENTRY_NUM)(0.U.asTypeOf(new tlb_t)))

    // for icache
    val i_tlb_hit       = WireDefault(VecInit.fill(TLB_ENTRY_NUM)(false.B))
    val i_tlb_entry     = Mux1H(i_tlb_hit, i_tlb)
    val i_tlb_hit_entry = TLB_Hit_Gen(i_tlb_entry, Mux(i_tlb_entry.ps(3), io.i_vaddr(12), io.i_vaddr(21)))

    for(i <- 0 until TLB_ENTRY_NUM){
        val i_tlb_vppn  = Mux(i_tlb(i).ps(3), i_tlb(i).vppn, i_tlb(i).vppn(18, 10) ## 0.U(10.W))
        val i_vppn      = Mux(i_tlb(i).ps(3), io.i_vaddr(31, 13), io.i_vaddr(31, 23) ## 0.U(10.W))
        i_tlb_hit(i)   := ((i_tlb(i).e && !(i_tlb_vppn ^ i_vppn))
                        && (i_tlb(i).g || !(i_tlb(i).asid ^ RegNext(io.csr_asid))))
    }
    io.i_tlb_uncache   := i_tlb_hit_entry.mat(0)
    io.i_tlb_paddr     := Mux(i_tlb_hit_entry.ps(3), 
                          Cat(i_tlb_hit_entry.ppn, io.i_vaddr(11, 0)),
                          Cat(i_tlb_hit_entry.ppn(19, 9), io.i_vaddr(20, 0)))
    
    val i_tlb_hit_reg       = reg1(i_tlb_hit, io.i_stall)
    val i_tlb_hit_entry_reg = reg1(i_tlb_hit_entry, io.i_stall)
    val i_csr_plv_reg       = reg1(io.csr_plv, io.i_stall)
    val i_valid_reg         = reg1(io.i_valid, io.i_stall)
    io.i_tlb_exception     := Signal_Exception(i_tlb_hit_reg.asUInt.orR, i_tlb_hit_entry_reg, i_csr_plv_reg, i_valid_reg, false.B, false.B)
    
    // for dcache
    val d_tlb_hit       = WireDefault(VecInit.fill(TLB_ENTRY_NUM)(false.B))
    val d_tlb_entry     = Mux1H(reg1(d_tlb_hit, io.d_stall), d_tlb)
    val d_tlb_hit_entry = TLB_Hit_Gen(d_tlb_entry, Mux(d_tlb_entry.ps(3), 
                        reg1(io.d_vaddr(12), io.d_stall), reg1(io.d_vaddr(21), io.d_stall)))

    for(i <- 0 until TLB_ENTRY_NUM){
        val d_tlb_vppn  = Mux(d_tlb(i).ps(3), d_tlb(i).vppn, d_tlb(i).vppn(18, 10) ## 0.U(10.W))
        val d_vppn      = Mux(d_tlb(i).ps(3), io.d_vaddr(31, 13), io.d_vaddr(31, 23) ## 0.U(10.W))
        d_tlb_hit(i)   := ((d_tlb(i).e && !(d_tlb_vppn ^ d_vppn))
                        && (d_tlb(i).g || !(d_tlb(i).asid ^ RegNext(io.csr_asid))))
    }
    io.d_tlb_uncache   := d_tlb_hit_entry.mat(0)
    io.d_tlb_paddr     := Mux(d_tlb_hit_entry.ps(3), 
                        Cat(d_tlb_hit_entry.ppn, reg1(io.d_vaddr(11, 0), io.d_stall)),
                        Cat(d_tlb_hit_entry.ppn(19, 9), reg1(io.d_vaddr(20, 0), io.d_stall)))

    val d_tlb_hit_reg       = reg1(d_tlb_hit, io.d_stall)
    val d_tlb_hit_entry_reg = d_tlb_hit_entry
    val d_csr_plv_reg       = reg1(io.csr_plv, io.d_stall)
    val d_rvalid_reg        = reg1(io.d_rvalid, io.d_stall)
    val d_wvalid_reg        = reg1(io.d_wvalid, io.d_stall)
    val d_paddr_reg         = io.d_tlb_paddr
    io.d_tlb_exception     := Mux(d_csr_plv_reg === 3.U && d_paddr_reg(31), 1.U(1.W) ## ADEM, 
                            Signal_Exception(d_tlb_hit_reg.asUInt.orR, d_tlb_hit_entry_reg, 
                            d_csr_plv_reg, false.B, d_rvalid_reg, d_wvalid_reg))

    // tlbsrch
    val csr_tlbehi_vppn   = io.csr_tlbehi
    val tlbsrch_hit       = WireDefault(VecInit.fill(TLB_ENTRY_NUM)(false.B))
    val tlbsrch_hit_idx   = OHToUInt(tlbsrch_hit)
    for(i <- 0 until TLB_ENTRY_NUM){
        val tlb_vppn    = Mux(d_tlb(i).ps(3), d_tlb(i).vppn, d_tlb(i).vppn(18, 10) ## 0.U(10.W))
        val csr_vppn    = Mux(d_tlb(i).ps(3), csr_tlbehi_vppn, csr_tlbehi_vppn(18, 10) ## 0.U(10.W))
        tlbsrch_hit(i) := ((d_tlb(i).e && !(tlb_vppn ^ csr_vppn))
                        && (d_tlb(i).g || !(d_tlb(i).asid ^ io.csr_asid)))
    }
    io.tlbsrch_idx := tlbsrch_hit_idx
    io.tlbsrch_hit := tlbsrch_hit.asUInt.orR

    // tlbrd
    io.tlbrd_entry := d_tlb(io.csr_tlbidx)

    // tlbwr and tlbfill
    when(io.tlbwr_en || io.tlbfill_en){
        val tlb_idx             = Mux(io.tlbwr_en, io.csr_tlbidx, io.tlbfill_idx)
        i_tlb(tlb_idx)          := io.tlbwr_entry
    }
    when(RegNext(io.tlbwr_en) || RegNext(io.tlbfill_en)){
        val tlb_idx             = Mux(RegNext(io.tlbwr_en), RegNext(io.csr_tlbidx), RegNext(io.tlbfill_idx))
        d_tlb(tlb_idx)          := RegNext(io.tlbwr_entry)
    }

    // invtlb for itlb
    val invtlb_asid = io.invtlb_asid
    val invtlb_vaddr = io.invtlb_vaddr

    when(io.invtlb_en){
        for(i <- 0 until TLB_ENTRY_NUM){
            switch(io.invtlb_op){
                is(0.U){   // clear all tlb entries
                    i_tlb(i).e := false.B
                }
                is(1.U){  // clear all tlb entries
                    i_tlb(i).e := false.B
                }
                is(2.U){  // clear (g = 1) entries
                    when(i_tlb(i).g){
                        i_tlb(i).e := false.B
                    }
                }
                is(3.U){  // clear (g = 0) entries
                    when(!i_tlb(i).g){
                        i_tlb(i).e := false.B
                    }
                }
                is(4.U){  // clear (asid = invtlb_asid && g = 0) entries
                    when(!(i_tlb(i).asid ^ invtlb_asid) && !i_tlb(i).g){
                        i_tlb(i).e := false.B
                    }
                }
                is(5.U){  // clear (asid = invtlb_asid && g = 0 && va[31:13] = invtlb_vaddr[31:13])
                    when(!(i_tlb(i).asid ^ invtlb_asid) && !i_tlb(i).g && !(i_tlb(i).vppn ^ invtlb_vaddr(31, 13))){
                        i_tlb(i).e := false.B
                    }
                }
                is(6.U){  // clear ((asid = invtlb_asid || g = 1) && va[31:13] = invtlb_vaddr[31:13])
                    when((!(i_tlb(i).asid ^ invtlb_asid) || i_tlb(i).g) && !(i_tlb(i).vppn ^ invtlb_vaddr(31, 13))){
                        i_tlb(i).e := false.B
                    }
                }
            }
        }
    }

    // invtlb for dtlb
    when(RegNext(io.invtlb_en)){
        for(i <- 0 until TLB_ENTRY_NUM){
            switch(RegNext(io.invtlb_op)){
                is(0.U){
                    d_tlb(i).e := false.B
                }
                is(1.U){
                    d_tlb(i).e := false.B
                }
                is(2.U){
                    when(d_tlb(i).g){
                        d_tlb(i).e := false.B
                    }
                }
                is(3.U){
                    when(!d_tlb(i).g){
                        d_tlb(i).e := false.B
                    }
                }
                is(4.U){
                    when(!(d_tlb(i).asid ^ RegNext(invtlb_asid)) && !d_tlb(i).g){
                        d_tlb(i).e := false.B
                    }
                }
                is(5.U){
                    when(!(d_tlb(i).asid ^ RegNext(invtlb_asid)) && !d_tlb(i).g && !(d_tlb(i).vppn ^ RegNext(invtlb_vaddr(31, 13)))){
                        d_tlb(i).e := false.B
                    }
                }
                is(6.U){
                    when((!(d_tlb(i).asid ^ RegNext(invtlb_asid)) || d_tlb(i).g) && !(d_tlb(i).vppn ^ RegNext(invtlb_vaddr(31, 13)))){
                        d_tlb(i).e := false.B
                    }
                }
            }
        }
    }
}

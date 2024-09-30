import chisel3._
import chisel3.util._

object control_signal{
    //alu op code
    val OP_ADD = 0.U(4.W)
    val OP_SUB = 1.U(4.W)
    val OP_SLT = 2.U(4.W)
    val OP_SLTU = 3.U(4.W)
    val OP_AND = 4.U(4.W)
    val OP_NOR = 5.U(4.W)
    val OP_OR = 6.U(4.W)
    val OP_XOR = 7.U(4.W)
    val OP_SLL = 8.U(4.W)
    val OP_SRL = 9.U(4.W)
    val OP_SRA = 10.U(4.W)
    val OP_MUL   = 0.U(4.W)
    val OP_MULH  = 1.U(4.W)
    val OP_MULHU = 2.U(4.W)
    val OP_DIV   = 4.U(4.W)
    val OP_MOD   = 5.U(4.W)
    val OP_DIVU  = 6.U(4.W)
    val OP_MODU  = 7.U(4.W)

    // ins_type
    val SYST     = 0.U(3.W)
    val MD       = 0.U(3.W)
    val RDCNT    = 1.U(3.W)
    val BR       = 2.U(3.W)
    val LS       = 3.U(3.W)
    val ARITH    = 4.U(3.W)

    // alu_rs1_sel
    val RS1_REG  = 0.U(1.W)
    val RS1_PC   = 1.U(1.W)

    // alu_rs2_sel
    val RS2_IMM  = 0.U(2.W)
    val RS2_REG  = 1.U(2.W)
    val RS2_FOUR = 2.U(2.W)
    val RS2_CNTH = 2.U(2.W)
    val RS2_CNTL = 3.U(2.W)

    // priv vec: last bit signed whether it is priv
    val NOT_PRIV = 0x0.U(13.W)
    val CSR_RD   = 0x1.U(13.W)
    val CSR_WR   = 0x3.U(13.W)    // bit 1
    val CSR_XCHG = 0x5.U(13.W)    // bit 2
    val PRV_ERET = 0x9.U(13.W)    // bit 3
    val TLB_RD   = 0x11.U(13.W)   // bit 4
    val TLB_WR   = 0x21.U(13.W)   // bit 5
    val TLB_FILL = 0x41.U(13.W)   // bit 6
    val TLB_SRCH = 0x81.U(13.W)   // bit 7
    val INV_TLB  = 0x101.U(13.W)  // bit 8
    val PRV_IDLE = 0x201.U(13.W)  // bit 9
    val CACHE_OP = 0x401.U(13.W)  // bit 10
    val LS_LL    = 0x801.U(13.W)  // bit 11
    val LS_SC    = 0x1001.U(13.W) // bit 12

    // mem_type
    val NO_MEM   = 0.U(5.W)
    val LDB      = 0x10.U(5.W)
    val LDH      = 0x11.U(5.W)
    val LDW      = 0x12.U(5.W)
    val STB      = 0x14.U(5.W)
    val STH      = 0x15.U(5.W)
    val STW      = 0x16.U(5.W)
    val LDBU     = 0x18.U(5.W)
    val LDHU     = 0x19.U(5.W)

    // br_type  inst(29, 26)
    val BR_JIRL = 3.U(4.W)
    val BR_B    = 4.U(4.W)
    val BR_BL   = 5.U(4.W)
    val BR_BEQ  = 6.U(4.W)
    val BR_BNE  = 7.U(4.W)
    val BR_BLT  = 8.U(4.W)
    val BR_BGE  = 9.U(4.W)
    val BR_BLTU = 10.U(4.W)
    val BR_BGEU = 11.U(4.W)

    // exception
    val NO_EXP    = 0.U(8.W)
    val SYS       = 0x8b.U(8.W)
    val BRK       = 0x8c.U(8.W)
    val INE       = 0x8d.U(8.W)
}

object CSR_CODE {
    val CSR_CRMD        = 0x0.U(14.W)
    val CSR_PRMD        = 0x1.U(14.W)
    val CSR_EUEN        = 0x2.U(14.W)
    val CSR_ECFG        = 0x4.U(14.W)
    val CSR_ESTAT       = 0x5.U(14.W)
    val CSR_ERA         = 0x6.U(14.W)
    val CSR_BADV        = 0x7.U(14.W)
    val CSR_EENTRY      = 0xc.U(14.W)
    val CSR_TLBIDX      = 0x10.U(14.W)
    val CSR_TLBEHI      = 0x11.U(14.W)
    val CSR_TLBELO0     = 0x12.U(14.W)
    val CSR_TLBELO1     = 0x13.U(14.W)
    val CSR_ASID        = 0x18.U(14.W)
    val CSR_PGDL        = 0x19.U(14.W)
    val CSR_PGDH        = 0x1a.U(14.W)
    val CSR_PGD         = 0x1b.U(14.W)
    val CSR_CPUID       = 0x20.U(14.W)
    val CSR_SAVE0       = 0x30.U(14.W)
    val CSR_SAVE1       = 0x31.U(14.W)
    val CSR_SAVE2       = 0x32.U(14.W)
    val CSR_SAVE3       = 0x33.U(14.W)
    val CSR_TID         = 0x40.U(14.W)
    val CSR_TCFG        = 0x41.U(14.W)
    val CSR_TVAL        = 0x42.U(14.W)
    val CSR_TICLR       = 0x44.U(14.W)
    val CSR_LLBCTL      = 0x60.U(14.W)
    val CSR_TLBRENTRY   = 0x88.U(14.W)
    val CSR_CTAG        = 0x98.U(14.W)
    val CSR_DMW0        = 0x180.U(14.W)
    val CSR_DMW1        = 0x181.U(14.W)
}

object TLB_Struct {
    import exception_code._
    class tlb_t extends Bundle{
        val vppn = UInt(19.W)
        val ps   = UInt(6.W)
        val g    = Bool()
        val asid = UInt(10.W)
        val e    = Bool()
        val ppn0 = UInt(20.W)
        val plv0 = UInt(2.W)
        val mat0 = UInt(2.W)
        val d0   = Bool()
        val v0   = Bool()
        val ppn1 = UInt(20.W)
        val plv1 = UInt(2.W)
        val mat1 = UInt(2.W)
        val d1   = Bool()
        val v1   = Bool()
    }
    class tlb_hit_t extends Bundle{
        val vppn = UInt(19.W)
        val ps   = UInt(6.W)
        val g    = Bool()
        val asid = UInt(10.W)
        val e    = Bool()
        val ppn  = UInt(20.W)
        val plv  = UInt(2.W)
        val mat  = UInt(2.W)
        val d    = Bool()
        val v    = Bool()
    }
    def TLB_Entry_Gen(vppn: UInt, ps: UInt, g: Bool, asid: UInt, e: Bool, ppn0: UInt, plv0: UInt, mat0: UInt, d0: Bool, v0: Bool, ppn1: UInt, plv1: UInt, mat1: UInt, d1: Bool, v1: Bool): tlb_t = {
        val tlb_entry = Wire(new tlb_t)
        tlb_entry.vppn := vppn
        tlb_entry.ps   := ps
        tlb_entry.g    := g
        tlb_entry.asid := asid
        tlb_entry.e    := e
        tlb_entry.ppn0 := ppn0
        tlb_entry.plv0 := plv0
        tlb_entry.mat0 := mat0
        tlb_entry.d0   := d0
        tlb_entry.v0   := v0
        tlb_entry.ppn1 := ppn1
        tlb_entry.plv1 := plv1
        tlb_entry.mat1 := mat1
        tlb_entry.d1   := d1
        tlb_entry.v1   := v1
        tlb_entry
    }
    def TLB_Hit_Gen(tlb_entry: tlb_t, last: Bool): tlb_hit_t = {
        val tlb_hit_entry = Wire(new tlb_hit_t)
        tlb_hit_entry.vppn := tlb_entry.vppn
        tlb_hit_entry.ps   := tlb_entry.ps
        tlb_hit_entry.g    := tlb_entry.g
        tlb_hit_entry.asid := tlb_entry.asid
        tlb_hit_entry.e    := tlb_entry.e
        tlb_hit_entry.ppn  := Mux(last, tlb_entry.ppn1, tlb_entry.ppn0)
        tlb_hit_entry.plv  := Mux(last, tlb_entry.plv1, tlb_entry.plv0)
        tlb_hit_entry.mat  := Mux(last, tlb_entry.mat1, tlb_entry.mat0)
        tlb_hit_entry.d    := Mux(last, tlb_entry.d1, tlb_entry.d0)
        tlb_hit_entry.v    := Mux(last, tlb_entry.v1, tlb_entry.v0)
        tlb_hit_entry
    }
    def Signal_Exception(tlb_hit: Bool, tlb_hit_entry: tlb_hit_t, csr_plv: UInt, i_valid: Bool, d_rvalid: Bool, d_wvalid: Bool): UInt = {
        val exception = WireDefault(0.U(8.W))
        when(!tlb_hit){
            exception := 1.U(1.W) ## TLBR
        }.elsewhen(!tlb_hit_entry.v){
            exception := Mux(i_valid, 1.U(1.W) ## PIF, Mux(d_rvalid, 1.U(1.W) ## PIL, Mux(d_wvalid, 1.U(1.W) ## PIS, 0.U)))
        }.elsewhen(csr_plv > tlb_hit_entry.plv){
            exception := 1.U(1.W) ## PPI
        }.elsewhen(d_wvalid && !tlb_hit_entry.d){
            exception := 1.U(1.W) ## PME
        }
        exception
    }
}

object CSR_REG {
    class CSR_REG extends Bundle {
    val crmd        = UInt(32.W)
    val prmd        = UInt(32.W)
    val estat       = UInt(32.W)
    val euen        = UInt(32.W)
    val ecfg        = UInt(32.W)
    val era         = UInt(32.W)
    val badv        = UInt(32.W)
    val eentry      = UInt(32.W)
    val cpuid       = UInt(32.W)
    val save0       = UInt(32.W)
    val save1       = UInt(32.W)
    val save2       = UInt(32.W)
    val save3       = UInt(32.W)
    val llbctl      = UInt(32.W)
    val tlbidx      = UInt(32.W)
    val tlbehi      = UInt(32.W)
    val tlbelo0     = UInt(32.W)
    val tlbelo1     = UInt(32.W)
    val asid        = UInt(32.W)
    val pgdl        = UInt(32.W)
    val pgdh        = UInt(32.W)
    val pgd         = UInt(32.W)
    val tlbrentry   = UInt(32.W)
    val dmw0        = UInt(32.W)
    val dmw1        = UInt(32.W)
    val tid         = UInt(32.W)
    val tcfg        = UInt(32.W)
    val tval        = UInt(32.W)
    val ticlr       = UInt(32.W)
}
}
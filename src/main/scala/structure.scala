import chisel3._
import chisel3.util._


object RAT {
    class rat_t extends Boudle {
        val arch_reg = UInt(5.W)
        val valif = Bool()
    }
}

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
}
import chisel3._
import chisel3.util._

object Instructions {
    // rdcnt
    def RDCNTIDW    = BitPat("b0000000000000000011000?????00000")
    def RDCNTVLW    = BitPat("b000000000000000001100000000?????")
    def RDCNTVHW    = BitPat("b000000000000000001100100000?????")
    
    // logic reg-reg
    def ADDW        = BitPat("b00000000000100000???????????????")
    def SUBW        = BitPat("b00000000000100010???????????????")
    def SLT         = BitPat("b00000000000100100???????????????")
    def SLTU        = BitPat("b00000000000100101???????????????")
    def NOR         = BitPat("b00000000000101000???????????????")
    def AND         = BitPat("b00000000000101001???????????????")
    def OR          = BitPat("b00000000000101010???????????????")
    def XOR         = BitPat("b00000000000101011???????????????")
    def SLLW        = BitPat("b00000000000101110???????????????")
    def SRLW        = BitPat("b00000000000101111???????????????")
    def SRAW        = BitPat("b00000000000110000???????????????")
    def MULW        = BitPat("b00000000000111000???????????????")
    def MULHW       = BitPat("b00000000000111001???????????????")
    def MULHWU      = BitPat("b00000000000111010???????????????")
    def DIVW        = BitPat("b00000000001000000???????????????")
    def MODW        = BitPat("b00000000001000001???????????????")
    def DIVWU       = BitPat("b00000000001000010???????????????")
    def MODWU       = BitPat("b00000000001000011???????????????")

    // else
    def BREAK       = BitPat("b00000000001010100???????????????")
    def SYSCALL     = BitPat("b00000000001010110???????????????")

    // logic reg-imm
    def SLLIW       = BitPat("b00000000010000001???????????????")
    def SRLIW       = BitPat("b00000000010001001???????????????")
    def SRAIW       = BitPat("b00000000010010001???????????????")
    def SLTI        = BitPat("b0000001000??????????????????????")
    def SLTUI       = BitPat("b0000001001??????????????????????")
    def ADDIW       = BitPat("b0000001010??????????????????????")
    def ANDI        = BitPat("b0000001101??????????????????????")
    def ORI         = BitPat("b0000001110??????????????????????")
    def XORI        = BitPat("b0000001111??????????????????????")

    // CSR
    def CSRRD       = BitPat("b00000100??????????????00000?????")
    def CSRWR       = BitPat("b00000100??????????????00001?????")
    def CSRXCHG     = BitPat("b00000100????????????????????????")

    // cacop
    def CACOP       = BitPat("b0000011000??????????????????????")

    // tlb
    def TLBSRCH     = BitPat("b00000110010010000010100000000000")
    def TLBRD       = BitPat("b00000110010010000010110000000000")
    def TLBWR       = BitPat("b00000110010010000011000000000000")
    def TLBFILL     = BitPat("b00000110010010000011010000000000")

    // priv
    def ERTN        = BitPat("b00000110010010000011100000000000")
    def IDLE        = BitPat("b00000110010010001???????????????")
    def INVTLB      = BitPat("b00000110010010011???????????????")

    // imm and pc
    def LU12IW      = BitPat("b0001010?????????????????????????")
    def PCADDU12I   = BitPat("b0001110?????????????????????????")

    // atmomic
    def LLW         = BitPat("b00100000????????????????????????")
    def SCW         = BitPat("b00100001????????????????????????")

    // load-store
    def LDB         = BitPat("b0010100000??????????????????????")
    def LDH         = BitPat("b0010100001??????????????????????")
    def LDW         = BitPat("b0010100010??????????????????????")
    def STB         = BitPat("b0010100100??????????????????????")
    def STH         = BitPat("b0010100101??????????????????????")
    def STW         = BitPat("b0010100110??????????????????????")
    def LDBU        = BitPat("b0010101000??????????????????????")
    def LDHU        = BitPat("b0010101001??????????????????????")
    
    // branch
    def JIRL        = BitPat("b010011??????????????????????????")
    def B           = BitPat("b010100??????????????????????????")
    def BL          = BitPat("b010101??????????????????????????")
    def BEQ         = BitPat("b010110??????????????????????????")
    def BNE         = BitPat("b010111??????????????????????????")    
    def BLT         = BitPat("b011000??????????????????????????")
    def BGE         = BitPat("b011001??????????????????????????")
    def BLTU        = BitPat("b011010??????????????????????????")
    def BGEU        = BitPat("b011011??????????????????????????")
}

object Control{
    val Y = true.B
    val N = false.B

    // alu_op
    val ALU_ADD   = 0.U(4.W)
    val ALU_SUB   = 1.U(4.W)
    val ALU_SLT   = 2.U(4.W)
    val ALU_SLTU  = 3.U(4.W)
    val ALU_NOR   = 4.U(4.W)
    val ALU_AND   = 5.U(4.W)
    val ALU_OR    = 6.U(4.W)
    val ALU_XOR   = 7.U(4.W)
    val ALU_SLL   = 8.U(4.W)
    val ALU_SRL   = 9.U(4.W)
    val ALU_SRA   = 10.U(4.W)
    val ALU_MUL   = 0.U(4.W)
    val ALU_MULH  = 1.U(4.W)
    val ALU_MULHU = 2.U(4.W)
    val ALU_DIV   = 4.U(4.W)
    val ALU_MOD   = 5.U(4.W)
    val ALU_DIVU  = 6.U(4.W)
    val ALU_MODU  = 7.U(4.W)


    // alu_rs1_sel
    val RS1_REG  = 0.U(1.W)
    val RS1_PC   = 1.U(1.W)

    // alu_rs2_sel
    val RS2_REG  = 0.U(2.W)
    val RS2_IMM  = 1.U(2.W)
    val RS2_FOUR = 2.U(2.W)
    val RS2_CNTH = 2.U(2.W)
    val RS2_CNTL = 3.U(2.W)

    // br_type
    val NO_BR    = 0.U(4.W)
    val BR_JIRL  = 3.U(4.W)
    val BR_B     = 4.U(4.W)
    val BR_BL    = 5.U(4.W)
    val BR_BEQ   = 6.U(4.W)
    val BR_BNE   = 7.U(4.W)
    val BR_BLT   = 8.U(4.W)
    val BR_BGE   = 9.U(4.W)
    val BR_BLTU  = 10.U(4.W)
    val BR_BGEU  = 11.U(4.W)

    // mem_type
    val NO_MEM   = 0.U(5.W)
    val MEM_LDB  = 8.U(5.W)
    val MEM_LDH  = 9.U(5.W)
    val MEM_LDW  = 10.U(5.W)
    val MEM_STB  = 16.U(5.W)
    val MEM_STH  = 17.U(5.W)
    val MEM_STW  = 18.U(5.W)
    val MEM_LDBU = 12.U(5.W)
    val MEM_LDHU = 13.U(5.W)

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

    // exception
    val NO_EXP    = 0.U(8.W)
    val SYS       = 0x8b.U(8.W)
    val BRK       = 0x8c.U(8.W)
    val INE       = 0x8d.U(8.W)

    // fu_id
    val RDCNT    = 0.U(3.W)
    val BR       = 1.U(3.W)
    val SYST     = 2.U(3.W)
    val MD       = 2.U(3.W)
    val LS       = 3.U(3.W)
    val ARITH    = 4.U(3.W)

    // rk_sel
    val RD       = 0.U(2.W)
    val RK       = 1.U(2.W)

    // rd_sel
    val R1       = 1.U(2.W)
    val RJ       = 2.U(2.W)

    // imm_type
    val IMM_00U   = 0.U(4.W)
    val IMM_05U   = 1.U(4.W)
    val IMM_12U   = 2.U(4.W)
    val IMM_12S   = 3.U(4.W)
    val IMM_16S   = 4.U(4.W)
    val IMM_20S   = 5.U(4.W)
    val IMM_26S   = 6.U(4.W)
    val IMM_CSR   = 7.U(4.W)
    val IMM_TID   = 8.U(4.W)
    val IMM_ERA   = 9.U(4.W)
    val IMM_COP   = 10.U(4.W)
    val IMM_14S   = 11.U(4.W)

}

object Decode_Map{
    import Instructions._
    import Control._
    val default = List(
    // rs1_valid rs2_valid rf_we, alu_op   alu_rs1_sel alu_rs2_sel br_type mem_type iq_id, rk_sel, rd_sel, imm_type, priv_vec, exception
        N,       N,        N,     ALU_ADD, RS1_REG,    RS2_IMM,    NO_BR,  NO_MEM,  SYST,  RK,     RD,     IMM_00U,  NOT_PRIV, INE)
    val map = Array(
        //                  0| 1| 2| 3|         4|        5|       6|       7|        8|      9|  10| 11|      12|       13|      
        RDCNTIDW    -> List(N, N, Y, ALU_MUL,   RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   SYST,  RD, RJ, IMM_TID, CSR_RD,   NO_EXP),
        RDCNTVLW    -> List(N, N, Y, ALU_ADD,   RS1_REG, RS2_CNTL, NO_BR,   NO_MEM,   RDCNT, RK, RD, IMM_00U, NOT_PRIV, NO_EXP),
        RDCNTVHW    -> List(N, N, Y, ALU_ADD,   RS1_REG, RS2_CNTH, NO_BR,   NO_MEM,   RDCNT, RK, RD, IMM_00U, NOT_PRIV, NO_EXP),
        ADDW        -> List(Y, Y, Y, ALU_ADD,   RS1_REG, RS2_REG,  NO_BR,   NO_MEM,   ARITH, RK, RD, IMM_00U, NOT_PRIV, NO_EXP),
        SUBW        -> List(Y, Y, Y, ALU_SUB,   RS1_REG, RS2_REG,  NO_BR,   NO_MEM,   ARITH, RK, RD, IMM_00U, NOT_PRIV, NO_EXP),
        SLT         -> List(Y, Y, Y, ALU_SLT,   RS1_REG, RS2_REG,  NO_BR,   NO_MEM,   ARITH, RK, RD, IMM_00U, NOT_PRIV, NO_EXP),
        SLTU        -> List(Y, Y, Y, ALU_SLTU,  RS1_REG, RS2_REG,  NO_BR,   NO_MEM,   ARITH, RK, RD, IMM_00U, NOT_PRIV, NO_EXP),
        NOR         -> List(Y, Y, Y, ALU_NOR,   RS1_REG, RS2_REG,  NO_BR,   NO_MEM,   ARITH, RK, RD, IMM_00U, NOT_PRIV, NO_EXP),
        AND         -> List(Y, Y, Y, ALU_AND,   RS1_REG, RS2_REG,  NO_BR,   NO_MEM,   ARITH, RK, RD, IMM_00U, NOT_PRIV, NO_EXP),
        OR          -> List(Y, Y, Y, ALU_OR,    RS1_REG, RS2_REG,  NO_BR,   NO_MEM,   ARITH, RK, RD, IMM_00U, NOT_PRIV, NO_EXP),
        XOR         -> List(Y, Y, Y, ALU_XOR,   RS1_REG, RS2_REG,  NO_BR,   NO_MEM,   ARITH, RK, RD, IMM_00U, NOT_PRIV, NO_EXP),
        SLLW        -> List(Y, Y, Y, ALU_SLL,   RS1_REG, RS2_REG,  NO_BR,   NO_MEM,   ARITH, RK, RD, IMM_00U, NOT_PRIV, NO_EXP),
        SRLW        -> List(Y, Y, Y, ALU_SRL,   RS1_REG, RS2_REG,  NO_BR,   NO_MEM,   ARITH, RK, RD, IMM_00U, NOT_PRIV, NO_EXP),
        SRAW        -> List(Y, Y, Y, ALU_SRA,   RS1_REG, RS2_REG,  NO_BR,   NO_MEM,   ARITH, RK, RD, IMM_00U, NOT_PRIV, NO_EXP),
        MULW        -> List(Y, Y, Y, ALU_MUL,   RS1_REG, RS2_REG,  NO_BR,   NO_MEM,   MD,    RK, RD, IMM_00U, NOT_PRIV, NO_EXP),
        MULHW       -> List(Y, Y, Y, ALU_MULH,  RS1_REG, RS2_REG,  NO_BR,   NO_MEM,   MD,    RK, RD, IMM_00U, NOT_PRIV, NO_EXP),
        MULHWU      -> List(Y, Y, Y, ALU_MULHU, RS1_REG, RS2_REG,  NO_BR,   NO_MEM,   MD,    RK, RD, IMM_00U, NOT_PRIV, NO_EXP),
        DIVW        -> List(Y, Y, Y, ALU_DIV,   RS1_REG, RS2_REG,  NO_BR,   NO_MEM,   MD,    RK, RD, IMM_00U, NOT_PRIV, NO_EXP),
        MODW        -> List(Y, Y, Y, ALU_MOD,   RS1_REG, RS2_REG,  NO_BR,   NO_MEM,   MD,    RK, RD, IMM_00U, NOT_PRIV, NO_EXP),
        DIVWU       -> List(Y, Y, Y, ALU_DIVU,  RS1_REG, RS2_REG,  NO_BR,   NO_MEM,   MD,    RK, RD, IMM_00U, NOT_PRIV, NO_EXP),
        MODWU       -> List(Y, Y, Y, ALU_MODU,  RS1_REG, RS2_REG,  NO_BR,   NO_MEM,   MD,    RK, RD, IMM_00U, NOT_PRIV, NO_EXP),
        BREAK       -> List(N, N, N, ALU_MUL,   RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   SYST,  RD, RD, IMM_00U, NOT_PRIV, BRK),
        SYSCALL     -> List(N, N, N, ALU_MUL,   RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   SYST,  RD, RD, IMM_00U, NOT_PRIV, SYS),
        SLLIW       -> List(Y, N, Y, ALU_SLL,   RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   ARITH, RK, RD, IMM_05U, NOT_PRIV, NO_EXP),
        SRLIW       -> List(Y, N, Y, ALU_SRL,   RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   ARITH, RK, RD, IMM_05U, NOT_PRIV, NO_EXP),
        SRAIW       -> List(Y, N, Y, ALU_SRA,   RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   ARITH, RK, RD, IMM_05U, NOT_PRIV, NO_EXP),
        SLTI        -> List(Y, N, Y, ALU_SLT,   RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   ARITH, RK, RD, IMM_12S, NOT_PRIV, NO_EXP),
        SLTUI       -> List(Y, N, Y, ALU_SLTU,  RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   ARITH, RK, RD, IMM_12S, NOT_PRIV, NO_EXP),
        ADDIW       -> List(Y, N, Y, ALU_ADD,   RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   ARITH, RK, RD, IMM_12S, NOT_PRIV, NO_EXP),
        ANDI        -> List(Y, N, Y, ALU_AND,   RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   ARITH, RK, RD, IMM_12U, NOT_PRIV, NO_EXP),
        ORI         -> List(Y, N, Y, ALU_OR,    RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   ARITH, RK, RD, IMM_12U, NOT_PRIV, NO_EXP),
        XORI        -> List(Y, N, Y, ALU_XOR,   RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   ARITH, RK, RD, IMM_12U, NOT_PRIV, NO_EXP),
        CSRRD       -> List(N, N, Y, ALU_MUL,   RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   SYST,  RD, RD, IMM_CSR, CSR_RD,   NO_EXP),
        CSRWR       -> List(N, Y, Y, ALU_MUL,   RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   SYST,  RD, RD, IMM_CSR, CSR_WR,   NO_EXP),
        CSRXCHG     -> List(Y, Y, Y, ALU_MUL,   RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   SYST,  RD, RD, IMM_CSR, CSR_XCHG, NO_EXP),
        CACOP       -> List(Y, N, N, ALU_ADD,   RS1_REG, RS2_IMM,  NO_BR,   MEM_LDB,  LS,    RK, RD, IMM_COP, CACHE_OP, NO_EXP),
        TLBSRCH     -> List(Y, Y, Y, ALU_MUL,   RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   SYST,  RD, RD, IMM_TID, TLB_SRCH, NO_EXP),
        TLBRD       -> List(Y, Y, Y, ALU_MUL,   RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   SYST,  RD, RD, IMM_00U, TLB_RD,   NO_EXP),
        TLBWR       -> List(Y, Y, Y, ALU_MUL,   RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   SYST,  RD, RD, IMM_00U, TLB_WR,   NO_EXP),
        TLBFILL     -> List(Y, Y, Y, ALU_MUL,   RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   SYST,  RD, RD, IMM_00U, TLB_FILL, NO_EXP),
        ERTN        -> List(N, N, N, ALU_MUL,   RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   SYST,  RD, RD, IMM_ERA, PRV_ERET, NO_EXP),
        IDLE        -> List(N, N, N, ALU_MUL,   RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   SYST,  RD, RD, IMM_00U, PRV_IDLE, NO_EXP),
        INVTLB      -> List(Y, Y, N, ALU_MUL,   RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   SYST,  RK, RD, IMM_COP, INV_TLB,  NO_EXP),
        LU12IW      -> List(N, N, Y, ALU_ADD,   RS1_REG, RS2_IMM,  NO_BR,   NO_MEM,   ARITH, RK, RD, IMM_20S, NOT_PRIV, NO_EXP),
        PCADDU12I   -> List(N, N, Y, ALU_ADD,   RS1_PC,  RS2_IMM,  NO_BR,   NO_MEM,   ARITH, RK, RD, IMM_20S, NOT_PRIV, NO_EXP),
        LLW         -> List(Y, N, Y, ALU_ADD,   RS1_REG, RS2_IMM,  NO_BR,   MEM_LDW,  LS,    RK, RD, IMM_14S, LS_LL,    NO_EXP),
        SCW         -> List(Y, Y, Y, ALU_ADD,   RS1_REG, RS2_IMM,  NO_BR,   MEM_STW,  LS,    RD, RD, IMM_14S, LS_SC,    NO_EXP),
        LDB         -> List(Y, N, Y, ALU_ADD,   RS1_REG, RS2_IMM,  NO_BR,   MEM_LDB,  LS,    RK, RD, IMM_12S, NOT_PRIV, NO_EXP),
        LDH         -> List(Y, N, Y, ALU_ADD,   RS1_REG, RS2_IMM,  NO_BR,   MEM_LDH,  LS,    RK, RD, IMM_12S, NOT_PRIV, NO_EXP),
        LDW         -> List(Y, N, Y, ALU_ADD,   RS1_REG, RS2_IMM,  NO_BR,   MEM_LDW,  LS,    RK, RD, IMM_12S, NOT_PRIV, NO_EXP),
        STB         -> List(Y, Y, N, ALU_ADD,   RS1_REG, RS2_IMM,  NO_BR,   MEM_STB,  LS,    RD, RD, IMM_12S, NOT_PRIV, NO_EXP),
        STH         -> List(Y, Y, N, ALU_ADD,   RS1_REG, RS2_IMM,  NO_BR,   MEM_STH,  LS,    RD, RD, IMM_12S, NOT_PRIV, NO_EXP),
        STW         -> List(Y, Y, N, ALU_ADD,   RS1_REG, RS2_IMM,  NO_BR,   MEM_STW,  LS,    RD, RD, IMM_12S, NOT_PRIV, NO_EXP),
        LDBU        -> List(Y, N, Y, ALU_ADD,   RS1_REG, RS2_IMM,  NO_BR,   MEM_LDBU, LS,    RK, RD, IMM_12S, NOT_PRIV, NO_EXP),
        LDHU        -> List(Y, N, Y, ALU_ADD,   RS1_REG, RS2_IMM,  NO_BR,   MEM_LDHU, LS,    RK, RD, IMM_12S, NOT_PRIV, NO_EXP),
        JIRL        -> List(Y, N, Y, ALU_ADD,   RS1_PC,  RS2_FOUR, BR_JIRL, NO_MEM,   BR,    RK, RD, IMM_16S, NOT_PRIV, NO_EXP),
        B           -> List(N, N, N, ALU_ADD,   RS1_REG, RS2_IMM,  BR_B,    NO_MEM,   BR,    RK, RD, IMM_26S, NOT_PRIV, NO_EXP),
        BL          -> List(N, N, Y, ALU_ADD,   RS1_PC,  RS2_FOUR, BR_BL,   NO_MEM,   BR,    RK, R1, IMM_26S, NOT_PRIV, NO_EXP),
        BEQ         -> List(Y, Y, N, ALU_ADD,   RS1_REG, RS2_IMM,  BR_BEQ,  NO_MEM,   BR,    RD, RD, IMM_16S, NOT_PRIV, NO_EXP),
        BNE         -> List(Y, Y, N, ALU_ADD,   RS1_REG, RS2_IMM,  BR_BNE,  NO_MEM,   BR,    RD, RD, IMM_16S, NOT_PRIV, NO_EXP),
        BLT         -> List(Y, Y, N, ALU_ADD,   RS1_REG, RS2_IMM,  BR_BLT,  NO_MEM,   BR,    RD, RD, IMM_16S, NOT_PRIV, NO_EXP),
        BGE         -> List(Y, Y, N, ALU_ADD,   RS1_REG, RS2_IMM,  BR_BGE,  NO_MEM,   BR,    RD, RD, IMM_16S, NOT_PRIV, NO_EXP),
        BLTU        -> List(Y, Y, N, ALU_ADD,   RS1_REG, RS2_IMM,  BR_BLTU, NO_MEM,   BR,    RD, RD, IMM_16S, NOT_PRIV, NO_EXP),
        BGEU        -> List(Y, Y, N, ALU_ADD,   RS1_REG, RS2_IMM,  BR_BGEU, NO_MEM,   BR,    RD, RD, IMM_16S, NOT_PRIV, NO_EXP),
    )
}

class DecodeIO extends Bundle{
    val inst            = Input(UInt(32.W))
    val rj              = Output(UInt(5.W))
    val rk              = Output(UInt(5.W))
    val rd              = Output(UInt(5.W))
    val rd_valid        = Output(Bool())

    val imm             = Output(UInt(32.W))
    val alu_op          = Output(UInt(5.W))
    val alu_rs1_sel     = Output(UInt(1.W))
    val alu_rs2_sel     = Output(UInt(2.W))
    val br_type         = Output(UInt(4.W))
    val mem_type        = Output(UInt(5.W))

    val priv_vec        = Output(UInt(13.W))

    val fu_id           = Output(UInt(3.W))
    val exception       = Output(UInt(8.W))
}
class Decode extends Module{ 
    val io = IO(new DecodeIO)
    val ctrl = ListLookup(io.inst, Decode_Map.default, Decode_Map.map)

    io.rj               := Mux(ctrl(0).asBool, io.inst(9, 5), 0.U(5.W))

    io.rk               := Mux(ctrl(1).asBool, Mux(ctrl(9)(0).asBool, io.inst(14, 10), io.inst(4, 0)), 0.U)

    io.rd               := MuxLookup(ctrl(10), io.inst(4, 0))(Seq(
                            Control.RD    -> io.inst(4, 0),
                            Control.R1    -> 1.U(5.W),
                            Control.RJ    -> io.inst(9, 5)))
    io.rd_valid         := ctrl(2) & io.rd =/= 0.U(5.W)

    io.alu_op           := ctrl(3)
    io.alu_rs1_sel      := ctrl(4)
    io.alu_rs2_sel      := ctrl(5)

    io.br_type          := ctrl(6)
    io.mem_type         := ctrl(7)

    io.priv_vec         := ctrl(12)
    
    io.fu_id            := ctrl(8)
    io.exception        := Mux(ctrl(12)(8) && io.inst(4, 0) >= 7.U, 1.U ## Control.INE, ctrl(13))


    def Imm_Gen(inst: UInt, imm_type: UInt): UInt = {
        val imm = Wire(UInt(32.W))
        import Control._
        imm := DontCare
        switch(imm_type) {
            is(IMM_00U)     { imm := 0.U(32.W) }
            is(IMM_05U)     { imm := Cat(0.U(27.W), inst(14, 10)) }
            is(IMM_12U)     { imm := Cat(0.U(20.W), inst(21, 10)) }
            is(IMM_12S)     { imm := Cat(Fill(20, inst(21)), inst(21, 10)) }
            is(IMM_14S)     { imm := Cat(Fill(18, inst(21)), inst(21, 10), 0.U(2.W)) }
            is(IMM_16S)     { imm := Cat(Fill(14, inst(25)), inst(25, 10), 0.U(2.W)) }
            is(IMM_20S)     { imm := Cat(inst(24, 5), 0.U(12.W)) }
            is(IMM_26S)     { imm := Cat(Fill(4, inst(9)), inst(9, 0), inst(25, 10), 0.U(2.W)) }
            is(IMM_CSR)     { imm := Cat(0.U(18.W), inst(23, 10)) }
            is(IMM_TID)     { imm := 0x40.U(32.W) }
            is(IMM_ERA)     { imm := 0x6.U(32.W) }
            is(IMM_COP)     { imm := Cat(Fill(15, inst(21)), inst(21, 10), inst(4, 0))}
        }
        imm
    }

    io.imm              := Imm_Gen(io.inst, ctrl(11))
}

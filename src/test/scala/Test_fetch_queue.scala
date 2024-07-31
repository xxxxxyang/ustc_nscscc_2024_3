/*import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.VecLiterals._
import chisel3.experimental.BundleLiterals
import InstPacks._
import scala.util.Random

class Test_fetch_queue extends AnyFlatSpec with ChiselScalatestTester {
    import InstPacks.pack_PD
    behavior of "TestModule"
    // test class body here
    it should "test" in {
        test(new Diff_test_fetch_queue){ c =>
            for( _ <- 0 until 1000){
                val flush = Random.nextBoolean().B
                val stall = Random.nextBoolean()
                val in_pack_1_inst_valid = Random.nextBoolean().B
                val in_pack_1_pc = Random.nextInt(1 << 32).U
                val in_pack_1_pred_valid = Random.nextBoolean().B
                val in_pack_1_pred_jump = Random.nextBoolean().B
                val in_pack_1_pred_npc = Random.nextInt(1 << 32).U
                val in_pack_1_exception = Random.nextInt(1 << 8).U
                val in_pack_1_br_cnt = Random.nextInt(1 << 2).U
                val in_pack_1_inst = Random.nextInt(1 << 32).U
                val in_pack_2_inst_valid = Random.nextBoolean().B
                val in_pack_2_pc = Random.nextInt(1 << 32).U
                val in_pack_2_pred_valid = Random.nextBoolean().B
                val in_pack_2_pred_jump = Random.nextBoolean().B
                val in_pack_2_pred_npc = Random.nextInt(1 << 32).U
                val in_pack_2_exception = Random.nextInt(1 << 8).U
                val in_pack_2_br_cnt = Random.nextInt(1 << 2).U
                val in_pack_2_inst = Random.nextInt(1 << 32).U

                c.clock.step(1)
                c.io.mod1.flush.poke(flush)
                c.io.mod1.stall.poke(stall)
                c.io.mod2.flush.poke(flush)
                c.io.mod2.next_read.poke(stall)
                c.io.mod1.in_pack(0).inst_valid.poke(in_pack_1_inst_valid)
                c.io.mod1.in_pack(0).pc.poke(in_pack_1_pc)
                c.io.mod1.in_pack(0).pred_valid.poke(in_pack_1_pred_valid)
                c.io.mod1.in_pack(0).pred_jump.poke(in_pack_1_pred_jump)
                c.io.mod1.in_pack(0).pred_npc.poke(in_pack_1_pred_npc)
                c.io.mod1.in_pack(0).exception.poke(in_pack_1_exception)
                c.io.mod1.in_pack(0).br_cnt.poke(in_pack_1_br_cnt)
                c.io.mod1.in_pack(0).inst.poke(in_pack_1_inst)
                c.io.mod2.insts_pack(0).pc.poke(in_pack_1_pc)
                c.io.mod2.insts_pack(0).inst.poke(in_pack_1_inst)
                c.io.mod2.insts_pack(0).inst_valid.poke(in_pack_1_inst_valid)
                c.io.mod2.insts_pack(0).predict_jump.poke(in_pack_1_pred_jump)
                c.io.mod2.insts_pack(0).pred_npc.poke(in_pack_1_pred_npc)
                c.io.mod2.insts_pack(0).exception.poke(in_pack_1_exception)
                c.io.mod1.in_pack(1).inst_valid.poke(in_pack_2_inst_valid)
                c.io.mod1.in_pack(1).pc.poke(in_pack_2_pc)
                c.io.mod1.in_pack(1).pred_valid.poke(in_pack_2_pred_valid)
                c.io.mod1.in_pack(1).pred_jump.poke(in_pack_2_pred_jump)
                c.io.mod1.in_pack(1).pred_npc.poke(in_pack_2_pred_npc)
                c.io.mod1.in_pack(1).exception.poke(in_pack_2_exception)
                c.io.mod1.in_pack(1).br_cnt.poke(in_pack_2_br_cnt)
                c.io.mod1.in_pack(1).inst.poke(in_pack_2_inst)
                c.io.mod2.insts_pack(1).pc.poke(in_pack_2_pc)
                c.io.mod2.insts_pack(1).inst.poke(in_pack_2_inst)
                c.io.mod2.insts_pack(1).inst_valid.poke(in_pack_2_inst_valid)
                c.io.mod2.insts_pack(1).predict_jump.poke(in_pack_2_pred_jump)
                c.io.mod2.insts_pack(1).pred_npc.poke(in_pack_2_pred_npc)
                c.io.mod2.insts_pack(1).exception.poke(in_pack_2_exception)

                c.io.diff.out_valid.expect(true.B)
                c.io.diff.full.expect(true.B)

            }
        }
    }
}

class Diff_test_fetch_queue extends Module{
    val io = IO(new Bundle{
        val mod1 = new Bundle {
            val in_pack             = Input(Vec(2, new pack_PD))
            val out_pack            = Output(Vec(2, new pack_PD))
            val out_valid           = Output(Bool())// 要么输出两个，要么不输出
            
            val flush               = Input(Bool())
            val stall               = Input(Bool()) // ~next_ready
            val full                = Output(Bool())
        }
        val mod2 = new Bundle {
            val insts_pack          = Input(Vec(2, new inst_pack_PD_t))

            val next_read          = Input(Bool())
            val insts_valid_decode  = Output(Vec(2, Bool()))
            val insts_pack_id       = Output(Vec(2, new inst_pack_PD_t))
            
            val full                = Output(Bool())
            val flush               = Input(Bool())
        }
        val diff = new Bundle {
            val out_valid           = Output(Bool())
            //val out_pack            = Output(Vec(2, Bool()))
            val full               = Output(Bool())
        }
    })

    val mod1 = Module(new FetchQueue)
    val mod2 = Module(new Fetch_Queue)

    mod1.io <> io.mod1
    mod2.io <> io.mod2

    io.diff.out_valid := (mod1.io.out_valid === mod2.io.insts_valid_decode(0)) && (mod1.io.out_valid === mod2.io.insts_valid_decode(1))
    io.diff.full := io.mod1.full === io.mod2.full
}

class inst_pack_PD_t extends Bundle{
    val pc              = UInt(32.W)
    val inst            = UInt(32.W)
    val inst_valid      = Bool()
    val predict_jump    = Bool()
    val pred_npc        = UInt(32.W)
    val exception       = UInt(8.W)
}

class Fetch_Queue_IO extends Bundle{
    val insts_pack          = Input(Vec(2, new inst_pack_PD_t))

    val next_read          = Input(Bool())
    val insts_valid_decode  = Output(Vec(2, Bool()))
    val insts_pack_id       = Output(Vec(2, new inst_pack_PD_t))
    
    val full                = Output(Bool())
    val flush               = Input(Bool())
}

class Fetch_Queue extends Module{
    val io = IO(new Fetch_Queue_IO)
    val next_ready = !io.next_read
    /* config */
    val ROW_WIDTH = 16 / 2
    val queue = RegInit(VecInit.fill(2)(VecInit.fill(ROW_WIDTH)(0.U.asTypeOf(new inst_pack_PD_t))))

    def shift_add1(x: UInt): UInt = {
        val n = x.getWidth
        x(n-2, 0) ## x(n-1)
    }
    def shift_add2(x: UInt): UInt = {
        val n = x.getWidth
        x(n-3, 0) ## x(n-1, n-2)
    }
    val head = RegInit(1.U(ROW_WIDTH.W))
    val tail = RegInit(1.U(16.W))
    val tail_odd = VecInit.tabulate(ROW_WIDTH)(i => tail(2*i+1)).asUInt 
    val tail_even = VecInit.tabulate(ROW_WIDTH)(i => tail(2*i)).asUInt
    val tail_for_compare = tail_odd | tail_even

    val full = (head & shift_add1(tail_for_compare)).orR
    val empty = (head & tail_for_compare).orR

    // Enqueue
    io.full := full

    // calculate the entry index for each instruction
    val write_mask = tail & Fill(16, io.insts_pack(0).inst_valid) | shift_add1(tail) & Fill(16, io.insts_pack(1).inst_valid)
    val write_mask_odd = VecInit.tabulate(ROW_WIDTH)(i => write_mask(2*i+1)).asUInt
    val write_mask_even = VecInit.tabulate(ROW_WIDTH)(i => write_mask(2*i)).asUInt

    val write_data_even = Mux(tail_even.orR, io.insts_pack(0), io.insts_pack(1))
    val write_data_odd = Mux(!tail_even.orR, io.insts_pack(0), io.insts_pack(1))



    // even queue write
    for(i <- 0 until ROW_WIDTH){
        when(!full && write_mask_even(i)){
            queue(0)(i) := write_data_even
        }
    }
    // odd queue write
    for(i <- 0 until ROW_WIDTH){
        when(!full && write_mask_odd(i)){
            queue(1)(i) := write_data_odd
        }
    }

    // Dequeue
    for(i <- 0 until 2){
        io.insts_pack_id(i) := Mux1H(head, queue(i))
        io.insts_valid_decode(i) := !empty
    }
    // update ptrs
    when(!full){
        tail := Mux(io.insts_pack(0).inst_valid, Mux(io.insts_pack(1).inst_valid, shift_add2(tail), shift_add1(tail)), tail)
    }
    when(next_ready && !empty){
        head := shift_add1(head)
    }
    when(io.flush){
        head := 1.U
        tail := 1.U
    }


}*/

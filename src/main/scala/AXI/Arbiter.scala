import chisel3._
import chisel3.util._

class Arbiter_IO extends Bundle {
    // for ICache
    val i_araddr        = Input(UInt(32.W))
    val i_rvalid        = Input(Bool())
    val i_rready        = Output(Bool())
    val i_rdata         = Output(UInt(32.W))
    val i_rlast         = Output(Bool())
    val i_rsize         = Input(UInt(3.W))
    val i_rburst        = Input(UInt(2.W))
    val i_rlen          = Input(UInt(8.W))

    // for DCache
    val d_araddr        = Input(UInt(32.W))
    val d_rvalid        = Input(Bool())
    val d_rready        = Output(Bool())
    val d_rdata         = Output(UInt(32.W))
    val d_rlast         = Output(Bool())
    val d_rsize         = Input(UInt(3.W))
    val d_rburst        = Input(UInt(2.W))
    val d_rlen          = Input(UInt(8.W))
    val d_awaddr        = Input(UInt(32.W))
    val d_wvalid        = Input(Bool())
    val d_wready        = Output(Bool())
    val d_wdata         = Input(UInt(32.W))
    val d_wlast         = Input(Bool())
    val d_wsize         = Input(UInt(3.W))
    val d_wburst        = Input(UInt(2.W))
    val d_wlen          = Input(UInt(8.W))
    val d_wstrb         = Input(UInt(4.W))
    val d_bvalid        = Output(Bool())
    val d_bready        = Input(Bool())


    // for Main Memory
    val araddr          = Output(UInt(32.W))
    val arburst         = Output(UInt(2.W))         //读事务突发模式，直接转接
    val arid            = Output(UInt(4.W))         //读地址ID，目前空置
    val arlen           = Output(UInt(8.W))      
    val arready         = Input(Bool())
    val arsize          = Output(UInt(3.W))
    val arvalid         = Output(Bool())

    val awaddr          = Output(UInt(32.W))
    val awburst         = Output(UInt(2.W))         //写事务突发模式，直接转接
    val awid            = Output(UInt(4.W))         //写地址ID，目前空置
    val awlen           = Output(UInt(8.W))
    val awready         = Input(Bool())
    val awsize          = Output(UInt(3.W))
    val awvalid         = Output(Bool())

    val bid             = Input(UInt(4.W))          //写回ID，目前空置
    val bready          = Output(Bool())
    val bvalid          = Input(Bool())
    val bresp           = Input(UInt(2.W))          //写回响应，目前空置，包括正常（0b00）、
                                                    //错误（0b01）、保留（0b10）、未知（0b11）
    val rdata           = Input(UInt(32.W))
    val rid             = Input(UInt(4.W))          //读数据ID，目前空置
    val rlast           = Input(Bool())             //读响应，目前空置
    val rready          = Output(Bool())
    val rresp           = Input(UInt(2.W))
    val rvalid          = Input(Bool())

    val wdata           = Output(UInt(32.W))
    val wlast           = Output(Bool())
    val wready          = Input(Bool())
    val wstrb           = Output(UInt(4.W))         //写数据掩码，从d_wstrb中转接
    val wvalid          = Output(Bool())
}

class Arbiter extends Module{
    val io = IO(new Arbiter_IO)

    // default signals
    io.i_rready := false.B
    io.i_rdata  := io.rdata
    io.i_rlast  := false.B

    io.d_rready := false.B
    io.d_rdata  := io.rdata
    io.d_rlast  := false.B
    io.d_wready := false.B
    io.d_bvalid := false.B

    io.araddr   := io.i_araddr
    io.arburst  := io.i_rburst
    io.arid     := 0.U
    io.arlen    := io.i_rlen
    io.arsize   := io.i_rsize
    io.arvalid  := false.B

    io.awaddr   := io.d_awaddr
    io.awburst  := io.d_rburst
    io.awid     := 0.U
    io.awlen    := io.d_wlen
    io.awsize   := io.d_wsize
    io.awvalid  := false.B

    io.bready   := false.B

    io.rready   := false.B

    io.wdata    := io.d_wdata
    io.wlast    := false.B
    io.wstrb    := io.d_wstrb
    io.wvalid   := false.B

    // read FSM
    val r_idle :: r_iar :: r_ir :: r_dar :: r_dr :: Nil = Enum(5)

    val r_state = RegInit(r_idle)
    switch(r_state){
        is(r_idle){
            // idle state
            r_state := Mux(io.d_rvalid, r_dar, Mux(io.i_rvalid, r_iar, r_idle))
        }
        is(r_iar){
            // icache ar shake hand state
            io.arvalid  := true.B
            io.araddr   := io.i_araddr
            io.arburst  := io.i_rburst
            io.arsize   := io.i_rsize
            io.arlen    := io.i_rlen
            r_state     := Mux(io.arready, r_ir, r_iar)
        }
        is(r_dar){
            // dcache ar shake hand state
            io.arvalid  := true.B
            io.araddr   := io.d_araddr
            io.arburst  := io.d_rburst
            io.arsize   := io.d_rsize
            io.arlen    := io.d_rlen
            r_state     := Mux(io.arready, r_dr, r_dar)
        }
        is(r_ir){
            // icache read data state
            io.rready   := true.B
            io.i_rready := io.rvalid
            io.i_rdata  := io.rdata
            io.i_rlast  := io.rlast
            r_state     := Mux(io.rvalid && io.rlast, r_idle, r_ir)
        }
        is(r_dr){
            // dcache read data state
            io.rready   := true.B
            io.d_rready := io.rvalid
            io.d_rdata  := io.rdata
            io.d_rlast  := io.rlast
            r_state     := Mux(io.rvalid && io.rlast, r_idle, r_dr)
        }
    }

    // write FSM
    val w_idle :: w_daw :: w_dw :: w_db :: Nil = Enum(4)
    val w_state = RegInit(w_idle)
    switch(w_state){
        is(w_idle){
            // idle state
            w_state := Mux(io.d_wvalid, w_daw, w_idle)
        }
        is(w_daw){
            // dcache aw shake hand state
            io.awvalid  := true.B
            io.awaddr   := io.d_awaddr
            io.awburst  := io.d_wburst
            io.awsize   := io.d_wsize
            io.awlen    := io.d_wlen
            w_state     := Mux(io.awready, w_dw, w_daw)
        }
        is(w_dw){
            // dcache write data state
            io.wvalid   := true.B
            io.d_wready := io.wready
            io.wdata    := io.d_wdata
            io.wstrb    := io.d_wstrb
            io.wlast    := io.d_wlast
            w_state     := Mux(io.wready && io.d_wlast, w_db, w_dw)
        }
        is(w_db){
            // dcache write response state
            io.bready   := io.d_bready
            io.d_bvalid := io.bvalid
            w_state     := Mux(io.bvalid, w_idle, w_db)
        }
    }
}

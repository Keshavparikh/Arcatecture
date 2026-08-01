package lightningrv.lsu

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Load Queue (LoadQueue) - 16-Entry Speculative Load Tracking Queue
  */
class LQEntry(implicit config: ApexConfig) extends Bundle {
  val valid    = Bool()
  val pc       = UInt(64.W)
  val addr     = UInt(64.W)
  val prd      = UInt(log2Up(config.PRF_SIZE).W)
  val robIdx   = UInt(log2Up(config.ROB_ENTRIES).W)
  val executed = Bool()
}

class LoadQueue(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val allocReq    = Input(Bool())
    val allocUop    = Input(new MicroOp)
    val allocLqIdx  = Output(UInt(log2Up(config.LQ_ENTRIES).W))
    val isFull      = Output(Bool())

    val execValid   = Input(Bool())
    val execLqIdx   = Input(UInt(log2Up(config.LQ_ENTRIES).W))
    val execAddr    = Input(UInt(64.W))

    val commitValid = Input(Bool())
    val commitLqIdx = Input(UInt(log2Up(config.LQ_ENTRIES).W))

    val lqArray     = Output(Vec(config.LQ_ENTRIES, new LQEntry))
    val flush       = Input(Bool())
  })

  val lq    = RegInit(VecInit(Seq.fill(config.LQ_ENTRIES)(0.U.asTypeOf(new LQEntry))))
  val head  = RegInit(0.U(log2Up(config.LQ_ENTRIES).W))
  val tail  = RegInit(0.U(log2Up(config.LQ_ENTRIES).W))
  val count = RegInit(0.U(log2Up(config.LQ_ENTRIES + 1).W))

  io.isFull     := count >= config.LQ_ENTRIES.U
  io.allocLqIdx := tail
  io.lqArray    := lq

  when(io.allocReq && !io.isFull && !io.flush) {
    lq(tail).valid    := true.B
    lq(tail).pc       := io.allocUop.pc
    lq(tail).prd      := io.allocUop.prd
    lq(tail).robIdx   := io.allocUop.robIdx
    lq(tail).executed := false.B
    tail  := tail + 1.U
    count := count + 1.U
  }

  when(io.execValid) {
    lq(io.execLqIdx).addr     := io.execAddr
    lq(io.execLqIdx).executed := true.B
  }

  when(io.commitValid && lq(head).valid) {
    lq(head).valid := false.B
    head  := head + 1.U
    count := count - 1.U
  }

  when(io.flush) {
    tail  := head
    count := 0.U
    for (i <- 0 until config.LQ_ENTRIES) { lq(i).valid := false.B }
  }
}

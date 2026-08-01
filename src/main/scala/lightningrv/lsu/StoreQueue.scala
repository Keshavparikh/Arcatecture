package lightningrv.lsu

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Store Queue (StoreQueue) - 16-Entry Speculative Store Tracking Queue
  */
class SQEntry(implicit config: ApexConfig) extends Bundle {
  val valid    = Bool()
  val pc       = UInt(64.W)
  val addr     = UInt(64.W)
  val data     = UInt(64.W)
  val prs2     = UInt(log2Up(config.PRF_SIZE).W)
  val robIdx   = UInt(log2Up(config.ROB_ENTRIES).W)
  val addrValid= Bool()
  val dataValid= Bool()
}

class StoreQueue(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val allocReq    = Input(Bool())
    val allocUop    = Input(new MicroOp)
    val allocSqIdx  = Output(UInt(log2Up(config.SQ_ENTRIES).W))
    val isFull      = Output(Bool())

    val execAddrValid = Input(Bool())
    val execSqIdx     = Input(UInt(log2Up(config.SQ_ENTRIES).W))
    val execAddr      = Input(UInt(64.W))

    val execDataValid = Input(Bool())
    val execDataSqIdx = Input(UInt(log2Up(config.SQ_ENTRIES).W))
    val execData      = Input(UInt(64.W))

    val commitValid   = Input(Bool())
    val commitSqIdx   = Input(UInt(log2Up(config.SQ_ENTRIES).W))
    val committedSq   = Output(new SQEntry)

    val sqArray       = Output(Vec(config.SQ_ENTRIES, new SQEntry))
    val flush         = Input(Bool())
  })

  val sq    = RegInit(VecInit(Seq.fill(config.SQ_ENTRIES)(0.U.asTypeOf(new SQEntry))))
  val head  = RegInit(0.U(log2Up(config.SQ_ENTRIES).W))
  val tail  = RegInit(0.U(log2Up(config.SQ_ENTRIES).W))
  val count = RegInit(0.U(log2Up(config.SQ_ENTRIES + 1).W))

  io.isFull      := count >= config.SQ_ENTRIES.U
  io.allocSqIdx  := tail
  io.sqArray     := sq
  io.committedSq := sq(head)

  when(io.allocReq && !io.isFull && !io.flush) {
    sq(tail).valid     := true.B
    sq(tail).pc        := io.allocUop.pc
    sq(tail).prs2      := io.allocUop.prs2
    sq(tail).robIdx    := io.allocUop.robIdx
    sq(tail).addrValid := false.B
    sq(tail).dataValid := false.B
    tail  := tail + 1.U
    count := count + 1.U
  }

  when(io.execAddrValid) {
    sq(io.execSqIdx).addr      := io.execAddr
    sq(io.execSqIdx).addrValid := true.B
  }

  when(io.execDataValid) {
    sq(io.execDataSqIdx).data      := io.execData
    sq(io.execDataSqIdx).dataValid := true.B
  }

  when(io.commitValid && sq(head).valid) {
    sq(head).valid := false.B
    head  := head + 1.U
    count := count - 1.U
  }

  when(io.flush) {
    tail  := head
    count := 0.U
    for (i <- 0 until config.SQ_ENTRIES) { sq(i).valid := false.B }
  }
}

package lightningrv.issue

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Integer Issue Queue (IntIssueQueue) - 16-Entry Reservation Station
  */
class IntIssueQueue(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    // Dispatch Enqueue (4-Wide)
    val enqUops    = Input(Vec(config.DISPATCH_WIDTH, new MicroOp))
    val enqValid   = Input(Vec(config.DISPATCH_WIDTH, Bool()))
    val isFull     = Output(Bool())

    // Wakeup Broadcast Inputs (From Execution Writeback)
    val wakeupPrf  = Input(Vec(config.COMMIT_WIDTH, UInt(log2Up(config.PRF_SIZE).W)))
    val wakeupValid= Input(Vec(config.COMMIT_WIDTH, Bool()))

    // Issue Outputs to Dual Integer ALUs (Width: 2)
    val issueUops  = Output(Vec(config.ISSUE_WIDTH_INT, new MicroOp))
    val issueValid = Output(Vec(config.ISSUE_WIDTH_INT, Bool()))

    val flush      = Input(Bool())
  })

  val queue = RegInit(VecInit(Seq.fill(config.IQ_SIZE_INT)(0.U.asTypeOf(new MicroOp))))
  val busy  = RegInit(VecInit(Seq.fill(config.IQ_SIZE_INT)(false.B)))
  val rdy1  = RegInit(VecInit(Seq.fill(config.IQ_SIZE_INT)(false.B)))
  val rdy2  = RegInit(VecInit(Seq.fill(config.IQ_SIZE_INT)(false.B)))

  val freeEntries = busy.map(!_)
  io.isFull := PopCount(busy) > (config.IQ_SIZE_INT - config.DISPATCH_WIDTH).U

  // Wakeup Logic
  for (i <- 0 until config.IQ_SIZE_INT) {
    when(busy(i)) {
      for (w <- 0 until config.COMMIT_WIDTH) {
        when(io.wakeupValid(w) && io.wakeupPrf(w) =/= 0.U) {
          when(queue(i).prs1 === io.wakeupPrf(w)) { rdy1(i) := true.B }
          when(queue(i).prs2 === io.wakeupPrf(w)) { rdy2(i) := true.B }
        }
      }
    }
  }

  // Ready Selection via IssueArbiter
  val arbiter = Module(new IssueArbiter(config.IQ_SIZE_INT, config.ISSUE_WIDTH_INT))
  val isEntryReady = Wire(Vec(config.IQ_SIZE_INT, Bool()))
  for (i <- 0 until config.IQ_SIZE_INT) {
    isEntryReady(i) := busy(i) && rdy1(i) && rdy2(i)
  }
  arbiter.io.readyBits := isEntryReady

  for (w <- 0 until config.ISSUE_WIDTH_INT) {
    val sel = arbiter.io.issueSelect(w)
    io.issueValid(w) := arbiter.io.issueReq(w)
    io.issueUops(w)  := queue(sel)

    when(arbiter.io.issueReq(w) && !io.flush) {
      busy(sel) := false.B
    }
  }

  // Dispatch Enqueue
  for (d <- 0 until config.DISPATCH_WIDTH) {
    when(io.enqValid(d) && !io.isFull && !io.flush) {
      val freeIdx = PriorityEncoder(freeEntries)
      queue(freeIdx) := io.enqUops(d)
      busy(freeIdx)  := true.B
      rdy1(freeIdx)  := (io.enqUops(d).prs1 === 0.U)
      rdy2(freeIdx)  := (io.enqUops(d).prs2 === 0.U)
    }
  }

  when(io.flush) {
    for (i <- 0 until config.IQ_SIZE_INT) {
      busy(i) := false.B
    }
  }
}

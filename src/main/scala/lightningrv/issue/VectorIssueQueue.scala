package lightningrv.issue

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Vector Issue Queue (VectorIssueQueue) - 16-Entry SIMD Vector Reservation Station
  */
class VectorIssueQueue(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val enqUops    = Input(Vec(config.DISPATCH_WIDTH, new MicroOp))
    val enqValid   = Input(Vec(config.DISPATCH_WIDTH, Bool()))
    val isFull     = Output(Bool())

    val wakeupPrf  = Input(Vec(config.COMMIT_WIDTH, UInt(log2Up(config.PRF_SIZE).W)))
    val wakeupValid= Input(Vec(config.COMMIT_WIDTH, Bool()))

    val issueUop   = Output(new MicroOp)
    val issueValid = Output(Bool())

    val flush      = Input(Bool())
  })

  val queue = RegInit(VecInit(Seq.fill(config.IQ_SIZE_VEC)(0.U.asTypeOf(new MicroOp))))
  val busy  = RegInit(VecInit(Seq.fill(config.IQ_SIZE_VEC)(false.B)))
  val rdy1  = RegInit(VecInit(Seq.fill(config.IQ_SIZE_VEC)(false.B)))
  val rdy2  = RegInit(VecInit(Seq.fill(config.IQ_SIZE_VEC)(false.B)))

  val freeEntries = busy.map(!_)
  io.isFull := PopCount(busy) > (config.IQ_SIZE_VEC - config.DISPATCH_WIDTH).U

  for (i <- 0 until config.IQ_SIZE_VEC) {
    when(busy(i)) {
      for (w <- 0 until config.COMMIT_WIDTH) {
        when(io.wakeupValid(w) && io.wakeupPrf(w) =/= 0.U) {
          when(queue(i).prs1 === io.wakeupPrf(w)) { rdy1(i) := true.B }
          when(queue(i).prs2 === io.wakeupPrf(w)) { rdy2(i) := true.B }
        }
      }
    }
  }

  val arbiter = Module(new IssueArbiter(config.IQ_SIZE_VEC, 1))
  val isEntryReady = Wire(Vec(config.IQ_SIZE_VEC, Bool()))
  for (i <- 0 until config.IQ_SIZE_VEC) {
    isEntryReady(i) := busy(i) && rdy1(i) && rdy2(i)
  }
  arbiter.io.readyBits := isEntryReady

  val sel = arbiter.io.issueSelect(0)
  io.issueValid := arbiter.io.issueReq(0)
  io.issueUop   := queue(sel)

  when(arbiter.io.issueReq(0) && !io.flush) { busy(sel) := false.B }

  for (d <- 0 until config.DISPATCH_WIDTH) {
    when(io.enqValid(d) && !io.isFull && !io.flush && io.enqUops(d).isVector) {
      val freeIdx = PriorityEncoder(freeEntries)
      queue(freeIdx) := io.enqUops(d)
      busy(freeIdx)  := true.B
      rdy1(freeIdx)  := (io.enqUops(d).prs1 === 0.U)
      rdy2(freeIdx)  := (io.enqUops(d).prs2 === 0.U)
    }
  }

  when(io.flush) {
    for (i <- 0 until config.IQ_SIZE_VEC) { busy(i) := false.B }
  }
}

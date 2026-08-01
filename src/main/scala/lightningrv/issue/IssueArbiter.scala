package lightningrv.issue

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Issue Arbiter Module - Oldest-Ready Selection & Port Arbitration Engine
  */
class IssueArbiter(queueSize: Int, issueWidth: Int)(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val readyBits   = Input(Vec(queueSize, Bool()))
    val issueReq    = Output(Vec(issueWidth, Bool()))
    val issueSelect = Output(Vec(issueWidth, UInt(log2Up(queueSize).W)))
  })

  val selectedMask = Wire(Vec(queueSize, Bool()))
  for (i <- 0 until queueSize) {
    selectedMask(i) := false.B
  }

  for (w <- 0 until issueWidth) {
    val unselectedReady = Wire(Vec(queueSize, Bool()))
    for (i <- 0 until queueSize) {
      unselectedReady(i) := io.readyBits(i) && !selectedMask(i)
    }

    val hasReady = unselectedReady.asUInt.orR
    val selIdx   = PriorityEncoder(unselectedReady)

    io.issueReq(w)    := hasReady
    io.issueSelect(w) := selIdx

    when(hasReady) {
      selectedMask(selIdx) := true.B
    }
  }
}

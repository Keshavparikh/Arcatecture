package lightningrv.lsu

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Selective Replay Queue (ReplayQueue / SRQ) Engine
  * Replaces full 30+ instruction pipeline flushes on forwarding collisions by invalidating only dependent instructions.
  */
class ReplayQueue(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val collisionValid = Input(Bool())
    val failingPrd     = Input(UInt(log2Up(config.PRF_SIZE).W))
    val failingRobIdx  = Input(UInt(log2Up(config.ROB_ENTRIES).W))

    val replayPrdTag   = Output(UInt(log2Up(config.PRF_SIZE).W))
    val replayTrigger  = Output(Bool())
  })

  val activeReplayReg = RegInit(false.B)
  val replayPrdReg    = RegInit(0.U(log2Up(config.PRF_SIZE).W))

  when(io.collisionValid) {
    activeReplayReg := true.B
    replayPrdReg    := io.failingPrd
  }.otherwise {
    activeReplayReg := false.B
  }

  io.replayTrigger := activeReplayReg
  io.replayPrdTag  := replayPrdReg
}

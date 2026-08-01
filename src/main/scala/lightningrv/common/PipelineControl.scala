package lightningrv.common

import chisel3._
import chisel3.util._

/**
  * Global Pipeline Control & Flush Interconnect (v1.0 Frozen Specification)
  * Broadcasts branch mispredictions and exception rollback signals across all pipeline stages.
  */
class PipelineControl(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val redirectIn    = Input(new RedirectInfo)
    val exceptionIn   = Input(new ExceptionInfo)

    val globalFlush   = Output(Bool())
    val flushTarget   = Output(UInt(64.W))
    val flushRobIdx   = Output(UInt(log2Up(config.ROB_ENTRIES).W))
    val flushCkptIdx  = Output(UInt(log2Up(config.MAX_CHECKPOINTS).W))
    val isException   = Output(Bool())
  })

  val flushTriggered = io.redirectIn.valid || io.exceptionIn.valid

  io.globalFlush  := flushTriggered
  io.flushTarget  := Mux(io.exceptionIn.valid, io.exceptionIn.epc, io.redirectIn.targetPC)
  io.flushRobIdx  := io.redirectIn.robIdx
  io.flushCkptIdx := io.redirectIn.checkpointIdx
  io.isException  := io.exceptionIn.valid
}

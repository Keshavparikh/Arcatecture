package lightningrv.lsu

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Store Forwarding Unit (StoreForwardingUnit)
  * Checks speculative loads against pending stores in Store Queue; forwards data directly if addresses match.
  */
class StoreForwardingUnit(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val loadAddr   = Input(UInt(64.W))
    val loadSqIdx  = Input(UInt(log2Up(config.SQ_ENTRIES).W))
    val sqArray    = Input(Vec(config.SQ_ENTRIES, new SQEntry))

    val forwardHit = Output(Bool())
    val forwardData= Output(UInt(64.W))
  })

  val matches = Wire(Vec(config.SQ_ENTRIES, Bool()))
  for (i <- 0 until config.SQ_ENTRIES) {
    matches(i) := io.sqArray(i).valid && io.sqArray(i).addrValid && io.sqArray(i).dataValid && (io.sqArray(i).addr === io.loadAddr)
  }

  val forwardHit  = matches.asUInt.orR
  val matchIdx    = PriorityEncoder(matches)

  io.forwardHit  := forwardHit
  io.forwardData := Mux(forwardHit, io.sqArray(matchIdx).data, 0.U)
}

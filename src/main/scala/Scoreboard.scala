package lightningrv

import chisel3._
import chisel3.util._

/**
  * Register Scoreboard for RAW & WAW Hazard Tracking in DAE-Asymmetric Pipeline
  * Uses per-register in-flight reference counting to support multiple queued multi-cycle operations.
  */
class Scoreboard extends Module {
  val io = IO(new Bundle {
    // Slow Lane Multi-Cycle Reservation Input
    val reserveValid = Input(Bool())
    val reserveRd    = Input(UInt(5.W))

    // Write-Back Completion Clear Input
    val clearValid   = Input(Bool())
    val clearRd      = Input(UInt(5.W))

    // Hazard Status Outputs
    val isAnyBusy      = Output(Bool())
    val busyBitsOutput = Output(Vec(32, Bool()))
  })

  // 32 reference counters tracking in-flight multi-cycle destination registers
  val busyCounts = RegInit(VecInit(Seq.fill(32)(0.U(3.W))))
  val busyVec    = Wire(Vec(32, Bool()))

  for (i <- 0 until 32) {
    busyVec(i) := busyCounts(i) > 0.U
  }
  io.busyBitsOutput := busyVec

  // Scoreboard Reference Counting Logic (x0 is hardwired to 0)
  when(io.reserveValid && io.clearValid && io.reserveRd === io.clearRd && io.reserveRd =/= 0.U) {
    // Simultaneous reserve and clear on same register -> count unchanged
  }.elsewhen(io.reserveValid && io.reserveRd =/= 0.U) {
    when(busyCounts(io.reserveRd) < 7.U) {
      busyCounts(io.reserveRd) := busyCounts(io.reserveRd) + 1.U
    }
  }.elsewhen(io.clearValid && io.clearRd =/= 0.U) {
    when(busyCounts(io.clearRd) > 0.U) {
      busyCounts(io.clearRd) := busyCounts(io.clearRd) - 1.U
    }
  }

  // Pipeline Fence Status: Any register currently busy in Slow Lane
  io.isAnyBusy := busyCounts.asUInt =/= 0.U
}

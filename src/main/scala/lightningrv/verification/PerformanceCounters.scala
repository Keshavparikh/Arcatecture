package lightningrv.verification

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Hardware Performance Counters (PerformanceCounters) Module
  * Tracks hardware IPC, branch mispredictions, cache hit/miss rates, and vector utilization.
  */
class PerformanceCounters(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val retiredInsts = Input(UInt(3.W))
    val mispredict   = Input(Bool())
    val cacheMiss    = Input(Bool())

    val cycleCount   = Output(UInt(64.W))
    val instCount    = Output(UInt(64.W))
    val mispCount    = Output(UInt(64.W))
    val missCount    = Output(UInt(64.W))
  })

  val cycles = RegInit(0.U(64.W))
  val insts  = RegInit(0.U(64.W))
  val misps  = RegInit(0.U(64.W))
  val misses = RegInit(0.U(64.W))

  cycles := cycles + 1.U
  insts  := insts + io.retiredInsts
  when(io.mispredict) { misps := misps + 1.U }
  when(io.cacheMiss)  { misses := misses + 1.U }

  io.cycleCount := cycles
  io.instCount  := insts
  io.mispCount  := misps
  io.missCount  := misses
}

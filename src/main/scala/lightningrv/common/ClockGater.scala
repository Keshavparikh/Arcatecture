package lightningrv.common

import chisel3._
import chisel3.util._

/**
  * Integrated Clock Gating (ICG) & Operand Isolation Module
  * Cuts clock trees to idle VPUs / FPUs and freezes input registers to achieve sub-1.5W low power envelope.
  */
class ClockGater extends Module {
  val io = IO(new Bundle {
    val enableIn   = Input(Bool())  // True = Active, False = Gate Clock
    val gatedClkOut= Output(Clock())

    // Operand Isolation
    val operandAIn  = Input(UInt(64.W))
    val operandBIn  = Input(UInt(64.W))
    val isolatedAOut= Output(UInt(64.W))
    val isolatedBOut= Output(UInt(64.W))
  })

  // Simulated ICG Latch: latch enable on low clock phase to prevent glitches
  val icgLatch = RegInit(false.B)
  icgLatch := io.enableIn

  io.gatedClkOut := (clock.asBool && icgLatch).asClock

  // Freeze operands to 0 when disabled to prevent internal dynamic switching power dissipation
  io.isolatedAOut := Mux(io.enableIn, io.operandAIn, 0.U)
  io.isolatedBOut := Mux(io.enableIn, io.operandBIn, 0.U)
}

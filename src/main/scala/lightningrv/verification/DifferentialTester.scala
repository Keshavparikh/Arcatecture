package lightningrv.verification

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Differential Lockstep Tester (DifferentialTester)
  * Validates cycle-by-cycle state equivalence between LightningRV and reference simulator (Spike / QEMU).
  */
class DifferentialTester(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val rtlPC        = Input(UInt(64.W))
    val rtlReg       = Input(UInt(5.W))
    val rtlVal       = Input(UInt(64.W))
    val rtlValid     = Input(Bool())

    val refPC        = Input(UInt(64.W))
    val refReg       = Input(UInt(5.W))
    val refVal       = Input(UInt(64.W))
    val refValid     = Input(Bool())

    val mismatch     = Output(Bool())
  })

  val pcMatch  = !io.rtlValid || (io.rtlPC === io.refPC)
  val regMatch = !io.rtlValid || ((io.rtlReg === io.refReg) && (io.rtlVal === io.refVal))

  io.mismatch := io.rtlValid && io.refValid && (!pcMatch || !regMatch)
}

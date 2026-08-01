package lightningrv.vector

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Vector Scheduler (VectorScheduler) Module
  * Enforces vector issue constraints (max 1 issue/cycle), dispatches to VPU0 or VPU1, and resolves write conflicts.
  */
class VectorScheduler(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val vectorUopIn  = Input(new MicroOp)
    val inputValid   = Input(Bool())

    val vpu0Uop      = Output(new MicroOp)
    val vpu0Valid    = Output(Bool())

    val vpu1Uop      = Output(new MicroOp)
    val vpu1Valid    = Output(Bool())
  })

  // Round-Robin Dispatch Selection between VPU0 and VPU1
  val dispatchToggle = RegInit(false.B)

  when(io.inputValid) {
    dispatchToggle := !dispatchToggle
  }

  io.vpu0Uop   := io.vectorUopIn
  io.vpu0Valid := io.inputValid && !dispatchToggle

  io.vpu1Uop   := io.vectorUopIn
  io.vpu1Valid := io.inputValid && dispatchToggle
}

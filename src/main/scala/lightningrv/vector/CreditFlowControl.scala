package lightningrv.vector

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Credit-Based Flow Control (CreditFlowControl) Engine
  * Tracks vector credits to prevent vector command queue overflow deadlocks.
  */
class CreditFlowControl(maxCredits: Int = 16)(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val allocCredit = Input(Bool()) // Scalar dispatch enqueues vector command
    val freeCredit  = Input(Bool()) // Vector execution completes command

    val hasCredit   = Output(Bool())
    val creditCount = Output(UInt(log2Up(maxCredits + 1).W))
  })

  val credits = RegInit(maxCredits.U(log2Up(maxCredits + 1).W))

  io.hasCredit   := credits > 0.U
  io.creditCount := credits

  when(io.allocCredit && !io.freeCredit && (credits > 0.U)) {
    credits := credits - 1.U
  }.elsewhen(!io.allocCredit && io.freeCredit && (credits < maxCredits.U)) {
    credits := credits + 1.U
  }
}

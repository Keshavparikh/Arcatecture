package lightningrv.verification

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Hardware Debug CSR Interface (DebugCSR)
  */
class DebugCSR(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val debugReq  = Input(Bool())
    val debugData = Output(UInt(64.W))
  })

  val dscratch0 = RegInit(0.U(64.W))
  io.debugData := dscratch0
}

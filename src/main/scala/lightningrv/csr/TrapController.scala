package lightningrv.csr

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Trap Controller (TrapController) Module
  * Handles interrupts, page faults, illegal instructions, misaligned accesses, and precise global redirects.
  */
class TrapController(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val exceptionIn = Input(new ExceptionInfo)
    val timerIrq    = Input(Bool())
    val softwareIrq = Input(Bool())
    val externalIrq = Input(Bool())

    val trapValid   = Output(Bool())
    val trapCause   = Output(UInt(64.W))
    val redirectPC  = Output(UInt(64.W))
  })

  val hasIrq = io.timerIrq || io.softwareIrq || io.externalIrq
  val hasExc = io.exceptionIn.valid

  io.trapValid := hasIrq || hasExc
  io.trapCause := Mux(hasExc, io.exceptionIn.cause, Mux(io.timerIrq, "h8000000000000007".U, Mux(io.softwareIrq, "h8000000000000003".U, "h800000000000000B".U)))
  io.redirectPC:= Mux(hasExc, io.exceptionIn.epc, io.exceptionIn.epc)
}

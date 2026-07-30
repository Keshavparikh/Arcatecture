package lightningrv

import chisel3._
import chisel3.util._

/**
  * Core Local Interruptor (CLINT) Module
  * 
  * Specifications:
  * - `mtime`: 64-bit Real-Time Counter (increments every cycle).
  * - `mtimecmp`: 64-bit Timer Compare Register.
  * - `msip`: Software Interrupt Register.
  * - Triggers Machine/Supervisor Timer & Software Interrupts for Linux kernel tick scheduler.
  */
class CLINT extends Module {
  val io = IO(new Bundle {
    val busAddr   = Input(UInt(64.W))
    val busWData  = Input(UInt(64.W))
    val busWriteEn= Input(Bool())
    val busReadEn = Input(Bool())

    val busRData  = Output(UInt(64.W))

    val timerInterrupt    = Output(Bool())
    val softwareInterrupt = Output(Bool())
  })

  val mtimeReg    = RegInit(0.U(64.W))
  val mtimecmpReg = RegInit("hFFFFFFFFFFFFFFFF".U(64.W))
  val msipReg     = RegInit(0.U(64.W))

  // Real-Time Counter Increment
  mtimeReg := mtimeReg + 1.U

  // MMIO Register Map (CLINT Standards):
  // 0x02000000: msip
  // 0x02004000: mtimecmp
  // 0x0200BFF8: mtime
  val rData = WireDefault(0.U(64.W))
  when(io.busReadEn) {
    switch(io.busAddr(15, 0)) {
      is("h0000".U) { rData := msipReg }
      is("h4000".U) { rData := mtimecmpReg }
      is("hBFF8".U) { rData := mtimeReg }
    }
  }
  io.busRData := rData

  when(io.busWriteEn) {
    switch(io.busAddr(15, 0)) {
      is("h0000".U) { msipReg     := io.busWData }
      is("h4000".U) { mtimecmpReg := io.busWData }
      is("hBFF8".U) { mtimeReg    := io.busWData }
    }
  }

  io.timerInterrupt    := mtimeReg >= mtimecmpReg
  io.softwareInterrupt := msipReg(0)
}

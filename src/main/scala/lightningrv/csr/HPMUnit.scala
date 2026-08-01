package lightningrv.csr

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Hardware Performance Monitor Engine (HPMUnit)
  * Implements 29 64-bit counter CSRs (mhpmcounter3-31) and event selectors (mhpmevent3-31) for Linux perf tools.
  */
class HPMUnit(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val csrAddr      = Input(UInt(12.W))
    val csrWData     = Input(UInt(64.W))
    val csrWEn       = Input(Bool())
    val csrRData     = Output(UInt(64.W))

    // Hardware Event Pulse Matrix (Inputs from core stages)
    val eventPulses  = Input(Vec(29, Bool()))
    val scountovfIrq = Output(Bool())
  })

  // 29 64-bit Counter CSRs and 29 Event Selectors
  val counters = RegInit(VecInit(Seq.fill(29)(0.U(64.W))))
  val events   = RegInit(VecInit(Seq.fill(29)(0.U(64.W))))
  val inhibit  = RegInit(0.U(32.W)) // mcountinhibit CSR

  val overflow = RegInit(0.U(32.W)) // scountovf CSR
  io.scountovfIrq := overflow.orR

  // Counter Increments based on Event Pulse Matrix
  for (i <- 0 until 29) {
    val isInhibited = inhibit(i + 3)
    when(!isInhibited && io.eventPulses(i)) {
      val nextVal = counters(i) + 1.U
      counters(i) := nextVal
      when(counters(i)(63) === false.B && nextVal(63) === true.B) {
        overflow := overflow | (1.U << (i + 3))
      }
    }
  }

  // CSR Read / Write Routing (mhpmcounter3-31: 0xB03-0xB1F, mhpmevent3-31: 0x323-0x33F)
  val rData = WireDefault(0.U(64.W))
  when(io.csrAddr >= 0xB03.U && io.csrAddr <= 0xB1F.U) {
    rData := counters(io.csrAddr - 0xB03.U)
  }.elsewhen(io.csrAddr >= 0x323.U && io.csrAddr <= 0x33F.U) {
    rData := events(io.csrAddr - 0x323.U)
  }.elsewhen(io.csrAddr === 0x320.U) { // mcountinhibit
    rData := inhibit
  }.elsewhen(io.csrAddr === 0xDA0.U) { // scountovf
    rData := overflow
  }
  io.csrRData := rData

  when(io.csrWEn) {
    when(io.csrAddr >= 0xB03.U && io.csrAddr <= 0xB1F.U) {
      counters(io.csrAddr - 0xB03.U) := io.csrWData
    }.elsewhen(io.csrAddr >= 0x323.U && io.csrAddr <= 0x33F.U) {
      events(io.csrAddr - 0x323.U) := io.csrWData
    }.elsewhen(io.csrAddr === 0x320.U) {
      inhibit := io.csrWData(31, 0)
    }.elsewhen(io.csrAddr === 0xDA0.U) {
      overflow := io.csrWData(31, 0)
    }
  }
}

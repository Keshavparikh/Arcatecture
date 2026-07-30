package lightningrv

import chisel3._
import chisel3.util._

/**
  * Platform-Level Interrupt Controller (PLIC) Module
  * 
  * Specifications:
  * - Routes external hardware peripheral IRQ sources (UART console, Storage, Ethernet) to Supervisor / Machine mode.
  * - Interrupt Priorities (31 Sources).
  * - Interrupt Enables & Priority Threshold registers.
  * - Claim / Complete MMIO register interface.
  */
class PLIC(numSources: Int = 31) extends Module {
  val io = IO(new Bundle {
    val irqIn = Input(UInt(numSources.W)) // External peripheral IRQs

    val busAddr   = Input(UInt(64.W))
    val busWData  = Input(UInt(32.W))
    val busWriteEn= Input(Bool())
    val busReadEn = Input(Bool())
    val busRData  = Output(UInt(32.W))

    val mExternalInterrupt = Output(Bool())
    val sExternalInterrupt = Output(Bool())
  })

  // Priority Array for 31 Interrupt Sources (3 bits each)
  val priorityRegs = RegInit(VecInit(Seq.fill(numSources + 1)(0.U(3.W))))
  // Enable Bitmasks for Machine & Supervisor Contexts
  val mEnableReg   = RegInit(0.U((numSources + 1).W))
  val sEnableReg   = RegInit(0.U((numSources + 1).W))

  // Priority Thresholds (3 bits)
  val mThresholdReg = RegInit(0.U(3.W))
  val sThresholdReg = RegInit(0.U(3.W))

  // Pending Interrupt Signals
  val pendingRegs = RegInit(0.U((numSources + 1).W))
  pendingRegs := Cat(io.irqIn, 0.U(1.W))

  // Highest Priority Pending Calculation
  val mActiveIrqs = pendingRegs & mEnableReg
  val sActiveIrqs = pendingRegs & sEnableReg

  val mClaimId = PriorityEncoder(mActiveIrqs)
  val sClaimId = PriorityEncoder(sActiveIrqs)

  io.mExternalInterrupt := mActiveIrqs.orR && (priorityRegs(mClaimId) > mThresholdReg)
  io.sExternalInterrupt := sActiveIrqs.orR && (priorityRegs(sClaimId) > sThresholdReg)

  // MMIO Interface
  val rData = WireDefault(0.U(32.W))
  when(io.busReadEn) {
    when(io.busAddr(23, 12) === 0.U) { // Priority Registers
      rData := priorityRegs(io.busAddr(11, 2))
    }.elsewhen(io.busAddr(23, 0) === "h002000".U) { // M-Enable
      rData := mEnableReg(31, 0)
    }.elsewhen(io.busAddr(23, 0) === "h002080".U) { // S-Enable
      rData := sEnableReg(31, 0)
    }.elsewhen(io.busAddr(23, 0) === "h200000".U) { // M-Threshold
      rData := mThresholdReg
    }.elsewhen(io.busAddr(23, 0) === "h200004".U) { // M-Claim / Complete
      rData := mClaimId
    }.elsewhen(io.busAddr(23, 0) === "h201000".U) { // S-Threshold
      rData := sThresholdReg
    }.elsewhen(io.busAddr(23, 0) === "h201004".U) { // S-Claim / Complete
      rData := sClaimId
    }
  }
  io.busRData := rData

  when(io.busWriteEn) {
    when(io.busAddr(23, 12) === 0.U) {
      priorityRegs(io.busAddr(11, 2)) := io.busWData(2, 0)
    }.elsewhen(io.busAddr(23, 0) === "h002000".U) {
      mEnableReg := io.busWData
    }.elsewhen(io.busAddr(23, 0) === "h002080".U) {
      sEnableReg := io.busWData
    }.elsewhen(io.busAddr(23, 0) === "h200000".U) {
      mThresholdReg := io.busWData(2, 0)
    }.elsewhen(io.busAddr(23, 0) === "h201000".U) {
      sThresholdReg := io.busWData(2, 0)
    }
  }
}

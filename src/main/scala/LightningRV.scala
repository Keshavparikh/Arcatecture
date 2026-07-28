package lightningrv

import chisel3._
import chisel3.util._

/**
  * LightningRV Top-Level SoC Module (64-Bit Keshav-ISA Enabled)
  * 
  * Integrates Dual-Issue AGU with 16-entry 2-bit BTB, ComputeUnit with 64-Bit Banked Register File & Parallel
  * Dual-ALU Execution Engine, and 16KB Scratchpad SRAM Memory Array over a 64-bit instruction fetch bus.
  */
class LightningRV(memorySizeWords: Int = 4096, initWords: Seq[BigInt] = Seq()) extends Module {
  val io = IO(new Bundle {
    val trapHalt      = Output(Bool())
    val cycleCount    = Output(UInt(32.W))
    val instCount     = Output(UInt(32.W))
    val registerFile  = Output(Vec(32, UInt(64.W)))
    val mmioCharValid = Output(Bool())
    val mmioChar      = Output(UInt(8.W))
  })

  // Instantiate Submodules
  val scratchpad  = Module(new Scratchpad(memorySizeWords, initWords))
  val agu         = Module(new AGU)
  val computeUnit = Module(new ComputeUnit)

  // Wire Instruction Fetch (64-Bit Bus: 2 instructions per cycle)
  scratchpad.io.imemAddr := agu.io.pc
  agu.io.imemData64       := scratchpad.io.imemData64

  // Wire Dual Decoupled AGU -> ComputeUnit Handshakes
  computeUnit.io.in0 <> agu.io.out0
  computeUnit.io.in1 <> agu.io.out1

  // Wire Data Memory Interface
  scratchpad.io.dmemAddr        := computeUnit.io.dmemAddr
  scratchpad.io.dmemWriteEnable := computeUnit.io.dmemWriteEnable
  scratchpad.io.dmemWriteData   := computeUnit.io.dmemWriteData
  scratchpad.io.dmemFunct3      := computeUnit.io.dmemFunct3
  computeUnit.io.dmemReadData   := scratchpad.io.dmemReadData

  // Wire System Stall & Hazard Interlocks
  agu.io.fenceStall := computeUnit.io.fenceStall
  agu.io.trapHaltIn := computeUnit.io.trapHalt

  // Wire BTB Feedback Training Interface
  agu.io.branchRedirect  := computeUnit.io.branchRedirect
  agu.io.branchTarget    := computeUnit.io.branchTarget
  agu.io.btbUpdateValid  := computeUnit.io.btbUpdateValid
  agu.io.btbUpdatePC     := computeUnit.io.btbUpdatePC
  agu.io.btbUpdateTarget := computeUnit.io.btbUpdateTarget
  agu.io.btbUpdateTaken  := computeUnit.io.btbUpdateTaken

  // Wire System Outputs
  io.trapHalt      := computeUnit.io.trapHalt
  io.registerFile  := computeUnit.io.registerFile
  io.mmioCharValid := scratchpad.io.mmioCharValid
  io.mmioChar      := scratchpad.io.mmioChar

  // Hardware Cycle Counter & Retired Instruction Counter
  val cycleReg = RegInit(0.U(32.W))
  val instReg  = RegInit(0.U(32.W))

  when(!computeUnit.io.trapHalt) {
    cycleReg := cycleReg + 1.U
    val retiredCount = Mux(computeUnit.io.in0.fire && computeUnit.io.in1.fire, 2.U, Mux(computeUnit.io.in0.fire || computeUnit.io.wbValid, 1.U, 0.U))
    instReg := instReg + retiredCount
  }

  io.cycleCount := cycleReg
  io.instCount  := instReg
}

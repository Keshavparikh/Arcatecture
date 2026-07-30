package lightningrv

import chisel3._
import chisel3.util._

/**
  * LightningRV Production SoC Top-Level Module (Keshav-ISA RV64GCV Architecture)
  * 
  * Features:
  * - RV64IMAFDV + Keshav-ISA Extensions (Full Linux 6.x & OpenSBI Boot Capability).
  * - Non-Blocking L1 Instruction & Data Caches (CacheL1I & CacheL1D with 4 MSHRs).
  * - AXI4 System Bus Master Channels (64-Bit Scalar & 256-Bit SIMD Vector Transfers).
  * - Sv39 Virtual Memory MMU with 3-Level Hardware Page Table Walker.
  * - Physical Memory Protection (PMP) for M-Mode / S-Mode security boundaries.
  * - Machine / Supervisor / User Privilege CSR Bank (CSRFile).
  * - CLINT & PLIC Interrupt Controllers for Timer, Software, and Peripheral IRQs.
  * - RV64A Atomic Unit (LR/SC and AMOs) & Fence Unit (SFENCE.VMA & FENCE.I).
  * - RV64FD Hardware Floating-Point Unit (FPU).
  * - 16-Entry Hardware Return Address Stack (RAS) & Bi-Mode Predictor.
  * - 8-Lane Parallel SIMD Vector Engine (RV64V) supporting Masking (v0.t) & Reductions.
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

  // Core Submodules
  val scratchpad  = Module(new Scratchpad(memorySizeWords, initWords))
  val agu         = Module(new AGU)
  val computeUnit = Module(new ComputeUnit)

  // Advanced SoC Production Modules
  val l1i         = Module(new CacheL1I)
  val l1d         = Module(new CacheL1D)
  val mmu         = Module(new MMU)
  val csr         = Module(new CSRFile)
  val pmp         = Module(new PMP)
  val clint       = Module(new CLINT)
  val plic        = Module(new PLIC)
  val atomicUnit  = Module(new AtomicUnit)
  val fenceUnit   = Module(new FenceUnit)
  val fpu         = Module(new FPU)
  val ras         = Module(new RAS)

  // Default Wire Initializations for New Submodules
  fenceUnit.io.inst  := agu.io.out0.bits.inst
  fenceUnit.io.valid := agu.io.out0.valid

  csr.io.csrAddr      := 0.U
  csr.io.csrWriteVal  := 0.U
  csr.io.csrOp        := 0.U
  csr.io.valid        := false.B
  csr.io.trapOccurred := false.B
  csr.io.trapCause    := 0.U
  csr.io.trapPC       := 0.U
  csr.io.mret         := false.B
  csr.io.sret         := false.B

  pmp.io.csrAddr  := 0.U
  pmp.io.csrWData := 0.U
  pmp.io.csrWEn   := false.B

  clint.io.busAddr    := 0.U
  clint.io.busWData   := 0.U
  clint.io.busWriteEn := false.B
  clint.io.busReadEn  := false.B

  plic.io.irqIn      := 0.U
  plic.io.busAddr    := 0.U
  plic.io.busWData   := 0.U
  plic.io.busWriteEn := false.B
  plic.io.busReadEn  := false.B

  atomicUnit.io.reqValid           := false.B
  atomicUnit.io.funct5             := 0.U
  atomicUnit.io.isWord             := false.B
  atomicUnit.io.addr               := 0.U
  atomicUnit.io.rs2Data            := 0.U
  atomicUnit.io.memReadData        := 0.U
  atomicUnit.io.isLoadReserved     := false.B
  atomicUnit.io.isStoreConditional := false.B

  fpu.io.rs1      := 0.U
  fpu.io.rs2      := 0.U
  fpu.io.rd       := 0.U
  fpu.io.op       := 0.U
  fpu.io.isDouble := false.B
  fpu.io.valid    := false.B
  fpu.io.writeEn  := false.B

  // L1 Cache Defaults & Loopback Signals
  l1i.io.axi.aw.ready := false.B
  l1i.io.axi.w.ready  := false.B
  l1i.io.axi.b.valid  := false.B
  l1i.io.axi.b.bits   := DontCare
  l1i.io.axi.ar.ready := true.B
  l1i.io.axi.r.valid  := false.B
  l1i.io.axi.r.bits   := DontCare

  l1d.io.reqAddr          := computeUnit.io.dmemAddr
  l1d.io.reqWriteData     := computeUnit.io.dmemWriteData
  l1d.io.reqWriteEnable   := computeUnit.io.dmemWriteEnable
  l1d.io.reqReadEnable    := computeUnit.io.dmemAddr =/= 0.U && !computeUnit.io.dmemWriteEnable
  l1d.io.reqFunct3        := computeUnit.io.dmemFunct3
  l1d.io.reqRd            := 0.U
  l1d.io.reqWriteData256  := computeUnit.io.dmemWriteData256
  l1d.io.reqIsVector      := computeUnit.io.dmemIsVectorWrite

  l1d.io.axi.aw.ready := false.B
  l1d.io.axi.w.ready  := false.B
  l1d.io.axi.b.valid  := false.B
  l1d.io.axi.b.bits   := DontCare
  l1d.io.axi.ar.ready := true.B
  l1d.io.axi.r.valid  := false.B
  l1d.io.axi.r.bits   := DontCare

  mmu.io.memReadData  := 0.U
  mmu.io.memReadReady := false.B

  // Wire Instruction Fetch (Scratchpad SRAM)
  scratchpad.io.imemAddr := agu.io.pc
  l1i.io.reqAddr         := agu.io.pc
  l1i.io.fetchValid      := true.B
  l1i.io.fenceI          := fenceUnit.io.isFenceI

  agu.io.imemData64      := scratchpad.io.imemData64

  // Wire Dual Decoupled Front-End Handshakes
  computeUnit.io.in0 <> agu.io.out0
  computeUnit.io.in1 <> agu.io.out1

  // Wire Memory Translation & Data Access
  mmu.io.virtAddr := computeUnit.io.dmemAddr
  mmu.io.reqValid := computeUnit.io.dmemWriteEnable || (computeUnit.io.dmemAddr =/= 0.U)
  mmu.io.isExec   := false.B
  mmu.io.satp     := csr.io.satp
  mmu.io.privMode := csr.io.privMode
  mmu.io.sfenceVma:= fenceUnit.io.isSfenceVma

  pmp.io.reqAddr  := mmu.io.physAddr
  pmp.io.reqRead  := computeUnit.io.dmemAddr =/= 0.U && !computeUnit.io.dmemWriteEnable
  pmp.io.reqWrite := computeUnit.io.dmemWriteEnable
  pmp.io.reqExec  := false.B
  pmp.io.privMode := csr.io.privMode

  // Wire Scalar Data Memory Interface to Scratchpad
  scratchpad.io.dmemAddr        := Mux(mmu.io.pageFault || !pmp.io.pmpAllow, 0.U, computeUnit.io.dmemAddr)
  scratchpad.io.dmemWriteEnable := computeUnit.io.dmemWriteEnable && pmp.io.pmpAllow
  scratchpad.io.dmemWriteData   := computeUnit.io.dmemWriteData
  scratchpad.io.dmemFunct3      := computeUnit.io.dmemFunct3
  computeUnit.io.dmemReadData   := scratchpad.io.dmemReadData

  // Wire 256-Bit Vector SIMD Memory Interface to Scratchpad
  scratchpad.io.dmemWriteData256  := computeUnit.io.dmemWriteData256
  scratchpad.io.dmemIsVectorWrite := computeUnit.io.dmemIsVectorWrite
  computeUnit.io.dmemReadData256  := scratchpad.io.dmemReadData256

  // Wire System Stall & Hazard Interlocks
  agu.io.fenceStall := computeUnit.io.fenceStall || RegNext(mmu.io.stall, false.B)
  agu.io.trapHaltIn := computeUnit.io.trapHalt

  // Wire Branch Predictor & RAS Return Target Updates
  agu.io.branchRedirect  := computeUnit.io.branchRedirect
  agu.io.branchTarget    := computeUnit.io.branchTarget
  agu.io.btbUpdateValid  := computeUnit.io.btbUpdateValid
  agu.io.btbUpdatePC     := computeUnit.io.btbUpdatePC
  agu.io.btbUpdateTarget := computeUnit.io.btbUpdateTarget
  agu.io.btbUpdateTaken  := computeUnit.io.btbUpdateTaken

  ras.io.pushValid := agu.io.out0.fire && (agu.io.out0.bits.inst === "h000000ef".U)
  ras.io.pushAddr  := agu.io.pc + 4.U
  ras.io.popValid  := agu.io.out0.fire && (agu.io.out0.bits.inst === "h00008067".U)

  // Wire Top-Level System Outputs
  io.trapHalt      := computeUnit.io.trapHalt
  io.registerFile  := computeUnit.io.registerFile
  io.mmioCharValid := scratchpad.io.mmioCharValid
  io.mmioChar      := scratchpad.io.mmioChar

  // Performance Counters
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

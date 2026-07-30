package lightningrv

import chisel3._
import chisel3.util._

/**
  * Fence Unit (FenceUnit) - TLB & Cache Synchronization Engine
  * 
  * Features:
  * - Decodes `SFENCE.VMA` (TLB Invalidation on virtual memory page table updates).
  * - Decodes `FENCE.I` (Instruction Cache Invalidation on dynamic code generation / JIT).
  */
class FenceUnit extends Module {
  val io = IO(new Bundle {
    val inst = Input(UInt(32.W))
    val valid= Input(Bool())

    val isFenceI   = Output(Bool())
    val isSfenceVma= Output(Bool())
    val flushTLB   = Output(Bool())
    val flushICache= Output(Bool())
  })

  val opcode = io.inst(6, 0)
  val funct3 = io.inst(14, 12)
  val funct7 = io.inst(31, 25)

  val isFenceI    = io.valid && (opcode === "b0001111".U) && (funct3 === "b001".U)
  val isSfenceVma = io.valid && (opcode === "b1110011".U) && (funct3 === "b000".U) && (funct7 === "b0001001".U)

  io.isFenceI    := isFenceI
  io.isSfenceVma := isSfenceVma
  io.flushICache := isFenceI
  io.flushTLB    := isSfenceVma
}

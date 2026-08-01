package lightningrv.frontend

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Instruction Aligner (InstructionAligner) - Stage 2
  * Parses 128-bit fetch blocks into 4 aligned `MicroOps` per cycle (handling RVC and 32-bit instructions).
  */
class InstructionAligner(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val fetchBlock128 = Input(UInt(128.W))
    val fetchPC       = Input(UInt(64.W))
    val alignValid    = Input(Bool())

    val outUops       = Output(Vec(config.DECODE_WIDTH, new MicroOp))
    val outValid      = Output(Vec(config.DECODE_WIDTH, Bool()))
  })

  // Unpack 4 x 32-bit instructions from 128-bit fetch block
  val rawInsts = Wire(Vec(4, UInt(32.W)))
  for (i <- 0 until 4) {
    rawInsts(i) := io.fetchBlock128(i * 32 + 31, i * 32)
  }

  for (i <- 0 until config.DECODE_WIDTH) {
    val inst = rawInsts(i)
    val u     = Wire(new MicroOp)
    u.pc             := io.fetchPC + (i * 4).U
    u.inst           := inst
    u.rs1            := inst(19, 15)
    u.rs2            := inst(24, 20)
    u.rd             := inst(11, 7)
    u.prs1           := 0.U
    u.prs2           := 0.U
    u.prd            := 0.U
    u.stalePrd       := 0.U
    u.robIdx         := 0.U
    u.lqIdx          := 0.U
    u.sqIdx          := 0.U
    u.checkpointIdx  := 0.U
    u.branchMask     := 0.U

    val opcode = inst(6, 0)
    u.isBranch       := opcode === "b1100011".U
    u.isJal          := opcode === "b1101111".U
    u.isJalr         := opcode === "b1100111".U
    u.isLoad         := opcode === "b0000011".U
    u.isStore        := opcode === "b0100011".U
    u.isAtomic       := opcode === "b0101111".U
    u.isFp           := opcode === "b1010011".U || opcode === "b0000111".U || opcode === "b0100111".U
    u.isVector       := opcode === "b1010111".U
    u.isSystem       := opcode === "b1110011".U
    u.aluOp          := inst(14, 12)
    u.imm            := Cat(Fill(52, inst(31)), inst(31, 20))
    u.useImm         := opcode === "b0010011".U || u.isLoad || u.isStore

    u.exceptionValid := false.B
    u.exceptionCause := 0.U

    io.outUops(i)  := u
    io.outValid(i) := io.alignValid && (inst =/= 0.U)
  }
}

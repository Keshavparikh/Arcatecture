package lightningrv

import chisel3._
import chisel3.util._

/**
  * Return Address Stack (RAS) Module
  * 
  * Specifications:
  * - 16-Entry LIFO Push/Pop Hardware Stack.
  * - Tracks Subroutine Call return addresses (`jal` / `jalr` where `rd = x1` or `x5`).
  * - Predicts Subroutine Return targets (`jalr` where `rs1 = x1` or `x5`) with 0-cycle bubble overhead.
  */
class RAS(depth: Int = 16) extends Module {
  val io = IO(new Bundle {
    val pushValid = Input(Bool())
    val pushAddr  = Input(UInt(64.W))

    val popValid  = Input(Bool())
    val predictedAddr = Output(UInt(64.W))
    val empty     = Output(Bool())
  })

  val stack = RegInit(VecInit(Seq.fill(depth)(0.U(64.W))))
  val sp    = RegInit(0.U(log2Up(depth + 1).W))

  val empty = sp === 0.U
  io.empty := empty

  io.predictedAddr := Mux(!empty, stack(sp - 1.U), 0.U)

  when(io.pushValid && !io.popValid) {
    when(sp < depth.U) {
      stack(sp) := io.pushAddr
      sp        := sp + 1.U
    }
  }.elsewhen(io.popValid && !io.pushValid) {
    when(sp > 0.U) {
      sp := sp - 1.U
    }
  }.elsewhen(io.pushValid && io.popValid) {
    when(sp > 0.U) {
      stack(sp - 1.U) := io.pushAddr
    }
  }
}

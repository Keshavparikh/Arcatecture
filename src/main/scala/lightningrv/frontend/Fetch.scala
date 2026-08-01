package lightningrv.frontend

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * 128-Bit Instruction Fetch Unit (Fetch) - Stage 1
  * Reads 16-byte aligned instruction blocks (4 x 32-bit instructions or up to 8 x 16-bit compressed instructions).
  */
class Fetch(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val pc             = Input(UInt(64.W))
    val imemData128    = Input(UInt(128.W))
    val fetchBlock128  = Output(UInt(128.W))
    val fetchPC        = Output(UInt(64.W))
    val valid          = Output(Bool())
    val stall          = Input(Bool())
  })

  val pcReg = RegInit(0.U(64.W))
  when(!io.stall) {
    pcReg := io.pc
  }

  io.fetchBlock128 := io.imemData128
  io.fetchPC       := pcReg
  io.valid         := true.B
}

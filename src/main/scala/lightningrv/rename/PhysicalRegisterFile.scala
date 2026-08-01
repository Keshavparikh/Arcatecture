package lightningrv.rename

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * 96-Entry Physical Register File (PRF) - Phase 1 Implementation
  * Holds 32 architectural registers + 64 rename registers.
  */
class PhysicalRegisterFile(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    // 4 Read Ports (Dual-Issue Scalar / Vector Operand Reads)
    val readAddr = Input(Vec(4, UInt(log2Up(config.PRF_SIZE).W)))
    val readData = Output(Vec(4, UInt(64.W)))

    // 4 Write Ports (4-Wide Writeback / Commit Writes)
    val writeValid = Input(Vec(4, Bool()))
    val writeAddr  = Input(Vec(4, UInt(log2Up(config.PRF_SIZE).W)))
    val writeData  = Input(Vec(4, UInt(64.W)))

    val prfArray   = Output(Vec(config.PRF_SIZE, UInt(64.W)))
  })

  // 96 x 64-bit Physical Register Array (Register x0 / p0 is hardwired to 0)
  val prf = RegInit(VecInit(Seq.fill(config.PRF_SIZE)(0.U(64.W))))
  io.prfArray := prf

  // Read Operations (p0 always returns 0)
  for (i <- 0 until 4) {
    io.readData(i) := Mux(io.readAddr(i) === 0.U, 0.U, prf(io.readAddr(i)))
  }

  // Write Operations (p0 writes are ignored)
  for (i <- 0 until 4) {
    when(io.writeValid(i) && io.writeAddr(i) =/= 0.U) {
      prf(io.writeAddr(i)) := io.writeData(i)
    }
  }
}

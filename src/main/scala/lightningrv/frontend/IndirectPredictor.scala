package lightningrv.frontend

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Indirect Predictor (IndirectPredictor) - 64-Entry Indirect Jump Target Buffer
  * Predicts jump targets for virtual functions and jump tables (`jalr`).
  */
class IndirectPredictor(tableSize: Int = 64)(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val reqPC       = Input(UInt(64.W))
    val predictedPC = Output(UInt(64.W))
    val hit         = Output(Bool())

    val updateValid = Input(Bool())
    val updatePC    = Input(UInt(64.W))
    val updateTarget= Input(UInt(64.W))
  })

  val targets    = Mem(tableSize, UInt(64.W))
  val validArray = RegInit(VecInit(Seq.fill(tableSize)(false.B)))

  val reqIdx = io.reqPC(log2Up(tableSize) + 1, 2)
  io.hit         := validArray(reqIdx)
  io.predictedPC := Mux(validArray(reqIdx), targets(reqIdx), io.reqPC + 4.U)

  when(io.updateValid) {
    val upIdx = io.updatePC(log2Up(tableSize) + 1, 2)
    targets(upIdx)    := io.updateTarget
    validArray(upIdx) := true.B
  }
}

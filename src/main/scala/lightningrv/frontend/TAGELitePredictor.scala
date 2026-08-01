package lightningrv.frontend

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * TAGE-Lite Predictor (TAGELitePredictor) - Multi-Table Tagged Geometric Branch Predictor
  */
class TAGELitePredictor(tableSize: Int = 256)(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val reqPC       = Input(UInt(64.W))
    val predictTaken= Output(Bool())

    val updateValid = Input(Bool())
    val updatePC    = Input(UInt(64.W))
    val updateTaken = Input(Bool())
  })

  // Global Branch History Register (16 bits)
  val ghist = RegInit(0.U(16.W))

  // Base Bimodal Predictor Table (2-bit saturating counters)
  val bimodal = RegInit(VecInit(Seq.fill(tableSize)(1.U(2.W))))

  // Tagged Table (4-bit tag + 2-bit counter)
  val taggedCtr = RegInit(VecInit(Seq.fill(tableSize)(2.U(2.W))))
  val taggedTag = Mem(tableSize, UInt(4.W))

  val reqIdx = io.reqPC(log2Up(tableSize) + 1, 2)
  val tag    = (io.reqPC(5, 2) ^ ghist(3, 0))

  val bimodalPred = bimodal(reqIdx)(1)
  val taggedMatch = taggedTag(reqIdx) === tag
  val taggedPred  = taggedCtr(reqIdx)(1)

  io.predictTaken := Mux(taggedMatch, taggedPred, bimodalPred)

  when(io.updateValid) {
    val upIdx = io.updatePC(log2Up(tableSize) + 1, 2)
    ghist := Cat(ghist(14, 0), io.updateTaken)

    val currentBimodal = bimodal(upIdx)
    when(io.updateTaken) {
      when(currentBimodal < 3.U) { bimodal(upIdx) := currentBimodal + 1.U }
    }.otherwise {
      when(currentBimodal > 0.U) { bimodal(upIdx) := currentBimodal - 1.U }
    }
  }
}

package lightningrv.frontend

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Branch Target Buffer (BTB) - 512-Entry Direct Jump Target Predictor
  */
class BTB(numEntries: Int = 512)(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val reqPC       = Input(UInt(64.W))
    val hit         = Output(Bool())
    val targetPC    = Output(UInt(64.W))

    val updateValid = Input(Bool())
    val updatePC    = Input(UInt(64.W))
    val updateTarget= Input(UInt(64.W))
  })

  val btbTags    = Mem(numEntries, UInt(55.W))
  val btbTargets = Mem(numEntries, UInt(64.W))
  val validArray = RegInit(VecInit(Seq.fill(numEntries)(false.B)))

  val reqIdx = io.reqPC(log2Up(numEntries) + 1, 2)
  val reqTag = io.reqPC(63, log2Up(numEntries) + 2)

  val hit = validArray(reqIdx) && (btbTags(reqIdx) === reqTag)
  io.hit      := hit
  io.targetPC := Mux(hit, btbTargets(reqIdx), io.reqPC + 4.U)

  when(io.updateValid) {
    val upIdx = io.updatePC(log2Up(numEntries) + 1, 2)
    val upTag = io.updatePC(63, log2Up(numEntries) + 2)
    btbTags(upIdx)    := upTag
    btbTargets(upIdx) := io.updateTarget
    validArray(upIdx) := true.B
  }
}

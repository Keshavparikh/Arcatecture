package lightningrv.lsu

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Memory Dependence Predictor (MemoryDependencePredictor) - Store Sets Algorithm
  * Predicts load-store conflicts to prevent constant pipeline replay flushes.
  */
class MemoryDependencePredictor(tableSize: Int = 64)(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val loadPC      = Input(UInt(64.W))
    val predictWait = Output(Bool())

    // Train on Memory Disambiguation Violation (Conflict Detection)
    val trainValid  = Input(Bool())
    val trainLoadPC = Input(UInt(64.W))
    val trainStorePC= Input(UInt(64.W))
  })

  // Store Sets Table (64 entries mapping PC hashes to conflict wait state)
  val sst = RegInit(VecInit(Seq.fill(tableSize)(false.B)))

  val loadHash  = io.loadPC(log2Up(tableSize) + 1, 2)
  val trainHash = io.trainLoadPC(log2Up(tableSize) + 1, 2)

  io.predictWait := sst(loadHash)

  when(io.trainValid) {
    sst(trainHash) := true.B
  }
}

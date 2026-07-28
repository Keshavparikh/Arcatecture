package lightningrv

import chisel3._
import chisel3.util._

/**
  * 2-Bit Branch Target Buffer (BTB) Predictor
  * 
  * Features:
  * - 16-entry direct-mapped Branch Target Buffer table.
  * - 2-bit saturating counter state machine:
  *     3 (11) = Strongly Taken
  *     2 (10) = Weakly Taken
  *     1 (01) = Weakly Not Taken
  *     0 (00) = Strongly Not Taken
  * - Zero-latency prediction lookups during AGU instruction fetch.
  * - Feedback updates from Compute Unit on branch execution.
  */
class BranchPredictor(val numEntries: Int = 16) extends Module {
  val io = IO(new Bundle {
    // Fetch Phase Prediction Interface
    val fetchPC       = Input(UInt(32.W))
    val predictTaken  = Output(Bool())
    val predictTarget = Output(UInt(32.W))

    // Execution Phase BTB Update Interface
    val updateValid   = Input(Bool())
    val updatePC      = Input(UInt(32.W))
    val updateTarget  = Input(UInt(32.W))
    val updateTaken   = Input(Bool())
  })

  // BTB Tables
  val validTable   = RegInit(VecInit(Seq.fill(numEntries)(false.B)))
  val tagTable     = RegInit(VecInit(Seq.fill(numEntries)(0.U(32.W))))
  val targetTable  = RegInit(VecInit(Seq.fill(numEntries)(0.U(32.W))))
  val counterTable = RegInit(VecInit(Seq.fill(numEntries)(2.U(2.W)))) // Default: Weakly Taken

  // Direct-mapped index hash (using middle bits of PC)
  val indexBits = log2Ceil(numEntries)
  def getIndex(pc: UInt): UInt = pc(indexBits + 1, 2)

  val fetchIndex = getIndex(io.fetchPC)
  val hit        = validTable(fetchIndex) && (tagTable(fetchIndex) === io.fetchPC)

  // Predict taken if entry is valid, tag matches, and counter >= 2 ("10" or "11")
  io.predictTaken  := hit && (counterTable(fetchIndex) >= 2.U)
  io.predictTarget := targetTable(fetchIndex)

  // Execution Update Logic
  when(io.updateValid) {
    val updateIndex = getIndex(io.updatePC)
    validTable(updateIndex)  := true.B
    tagTable(updateIndex)    := io.updatePC
    targetTable(updateIndex) := io.updateTarget

    val currentCounter = counterTable(updateIndex)
    when(io.updateTaken) {
      when(currentCounter < 3.U) { counterTable(updateIndex) := currentCounter + 1.U }
    }.otherwise {
      when(currentCounter > 0.U) { counterTable(updateIndex) := currentCounter - 1.U }
    }
  }
}

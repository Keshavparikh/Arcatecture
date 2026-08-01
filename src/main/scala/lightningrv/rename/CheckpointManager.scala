package lightningrv.rename

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Checkpoint Manager Module - 1-Cycle Snapshot Recovery Engine
  * Stores up to 8 snapshots of Rename Map Table & Free List Head pointer for predicted branches.
  */
class CheckpointEntry(implicit config: ApexConfig) extends Bundle {
  val valid    = Bool()
  val mapTable = Vec(32, UInt(log2Up(config.PRF_SIZE).W))
  val freeHead = UInt(log2Up(config.FREE_LIST_ENTRIES).W)
}

class CheckpointManager(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    // Checkpoint Creation (Rename Stage)
    val createValid = Input(Bool())
    val currentMap  = Input(Vec(32, UInt(log2Up(config.PRF_SIZE).W)))
    val currentHead = Input(UInt(log2Up(config.FREE_LIST_ENTRIES).W))
    val allocatedIdx= Output(UInt(log2Up(config.MAX_CHECKPOINTS).W))
    val allocFull   = Output(Bool())

    // Misprediction Snapshot Restoration
    val restoreValid= Input(Bool())
    val restoreIdx  = Input(UInt(log2Up(config.MAX_CHECKPOINTS).W))

    val restoredMap = Output(Vec(32, UInt(log2Up(config.PRF_SIZE).W)))
    val restoredHead= Output(UInt(log2Up(config.FREE_LIST_ENTRIES).W))

    // Checkpoint Release on Branch Commit
    val releaseValid= Input(Bool())
    val releaseIdx  = Input(UInt(log2Up(config.MAX_CHECKPOINTS).W))
  })

  val checkpoints = RegInit(VecInit(Seq.fill(config.MAX_CHECKPOINTS)(0.U.asTypeOf(new CheckpointEntry))))

  val freeCheckpoints = checkpoints.map(!_.valid)
  io.allocFull    := freeCheckpoints.reduce(_ && _)
  val allocIdx    = PriorityEncoder(freeCheckpoints)
  io.allocatedIdx := allocIdx

  when(io.createValid && !io.allocFull) {
    checkpoints(allocIdx).valid    := true.B
    checkpoints(allocIdx).mapTable := io.currentMap
    checkpoints(allocIdx).freeHead := io.currentHead
  }

  when(io.releaseValid) {
    checkpoints(io.releaseIdx).valid := false.B
  }

  io.restoredMap  := checkpoints(io.restoreIdx).mapTable
  io.restoredHead := checkpoints(io.restoreIdx).freeHead
}

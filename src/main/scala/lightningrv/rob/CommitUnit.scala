package lightningrv.rob

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Commit Unit Module - Single authority for retiring instructions and reclaiming stale PRFs
  */
class CommitUnit(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val robHead      = Input(new ROBEntry)
    val robCommitReady = Input(Bool())
    val robCommitPop = Output(Bool())

    val deallocReq   = Output(Vec(config.COMMIT_WIDTH, Bool()))
    val deallocPrf   = Output(Vec(config.COMMIT_WIDTH, UInt(log2Up(config.PRF_SIZE).W)))

    val commitStores = Output(Vec(config.COMMIT_WIDTH, Bool()))
    val commitSqIdx  = Output(Vec(config.COMMIT_WIDTH, UInt(log2Up(config.SQ_ENTRIES).W)))

    val exceptionValid = Output(Bool())
    val exceptionCause = Output(UInt(64.W))
    val exceptionPC    = Output(UInt(64.W))
  })

  val canCommit = io.robCommitReady && io.robHead.valid

  io.robCommitPop := canCommit

  val exceptionFound = WireDefault(false.B)
  val excCause       = WireDefault(0.U(64.W))

  for (i <- 0 until config.COMMIT_WIDTH) {
    val slot = io.robHead.slots(i)
    val slotCommit = canCommit && slot.valid && slot.complete

    io.deallocReq(i)  := slotCommit && (slot.stalePrd =/= 0.U)
    io.deallocPrf(i)  := slot.stalePrd

    io.commitStores(i):= slotCommit && slot.isStore
    io.commitSqIdx(i) := slot.sqIdx

    when(slotCommit && slot.exceptionValid && !exceptionFound) {
      exceptionFound := true.B
      excCause       := slot.exceptionCause
    }
  }

  io.exceptionValid := exceptionFound
  io.exceptionCause := excCause
  io.exceptionPC    := io.robHead.pc
}

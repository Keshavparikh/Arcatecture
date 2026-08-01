package lightningrv.rob

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * 32-Entry Hierarchical Reorder Buffer (ROB) - Phase 2 Implementation
  * Each ROB row shares PC and branch metadata across 4 instruction slots.
  */
class ROBEntrySlot(implicit config: ApexConfig) extends Bundle {
  val valid          = Bool()
  val complete       = Bool()
  val inst           = UInt(32.W)
  val prd            = UInt(log2Up(config.PRF_SIZE).W)
  val stalePrd       = UInt(log2Up(config.PRF_SIZE).W)
  val isStore        = Bool()
  val sqIdx          = UInt(log2Up(config.SQ_ENTRIES).W)
  val lqIdx          = UInt(log2Up(config.LQ_ENTRIES).W)
  val exceptionValid = Bool()
  val exceptionCause = UInt(64.W)
}

class ROBEntry(implicit config: ApexConfig) extends Bundle {
  val valid          = Bool()
  val pc             = UInt(64.W)
  val checkpointIdx  = UInt(log2Up(config.MAX_CHECKPOINTS).W)
  val slots          = Vec(config.DISPATCH_WIDTH, new ROBEntrySlot)
}

class HierarchicalROB(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    // Dispatch Enqueue (4-Wide)
    val enqValid     = Input(Bool())
    val enqPC        = Input(UInt(64.W))
    val enqCkptIdx   = Input(UInt(log2Up(config.MAX_CHECKPOINTS).W))
    val enqUops      = Input(Vec(config.DISPATCH_WIDTH, new MicroOp))
    val allocRobIdx  = Output(UInt(log2Up(config.ROB_ENTRIES).W))
    val robFull      = Output(Bool())

    // Writeback Completion Updates (4-Wide)
    val wbInfo       = Input(Vec(config.COMMIT_WIDTH, new WritebackInfo))

    // Commit Dequeue Output (4-Wide)
    val commitHead   = Output(new ROBEntry)
    val commitReady  = Output(Bool())
    val commitPop    = Input(Bool())

    // Flush / Misprediction Rollback
    val flushValid   = Input(Bool())
    val flushRobIdx  = Input(UInt(log2Up(config.ROB_ENTRIES).W))
  })

  val rob   = RegInit(VecInit(Seq.fill(config.ROB_ENTRIES)(0.U.asTypeOf(new ROBEntry))))
  val head  = RegInit(0.U(log2Up(config.ROB_ENTRIES).W))
  val tail  = RegInit(0.U(log2Up(config.ROB_ENTRIES).W))
  val count = RegInit(0.U(log2Up(config.ROB_ENTRIES + 1).W))

  io.robFull     := count >= config.ROB_ENTRIES.U
  io.allocRobIdx := tail

  // Dispatch Enqueue
  when(io.enqValid && !io.robFull && !io.flushValid) {
    rob(tail).valid         := true.B
    rob(tail).pc            := io.enqPC
    rob(tail).checkpointIdx := io.enqCkptIdx

    for (i <- 0 until config.DISPATCH_WIDTH) {
      rob(tail).slots(i).valid          := io.enqUops(i).inst =/= 0.U
      rob(tail).slots(i).complete       := false.B
      rob(tail).slots(i).inst           := io.enqUops(i).inst
      rob(tail).slots(i).prd            := io.enqUops(i).prd
      rob(tail).slots(i).stalePrd       := io.enqUops(i).stalePrd
      rob(tail).slots(i).isStore        := io.enqUops(i).isStore
      rob(tail).slots(i).sqIdx          := io.enqUops(i).sqIdx
      rob(tail).slots(i).lqIdx          := io.enqUops(i).lqIdx
      rob(tail).slots(i).exceptionValid := io.enqUops(i).exceptionValid
      rob(tail).slots(i).exceptionCause := io.enqUops(i).exceptionCause
    }
    tail  := tail + 1.U
    count := count + 1.U
  }

  // Execution Writeback Completion Mark
  for (w <- 0 until config.COMMIT_WIDTH) {
    when(io.wbInfo(w).valid) {
      val rIdx = io.wbInfo(w).robIdx
      for (s <- 0 until config.DISPATCH_WIDTH) {
        when(rob(rIdx).slots(s).valid && (rob(rIdx).slots(s).prd === io.wbInfo(w).prd)) {
          rob(rIdx).slots(s).complete := true.B
        }
      }
    }
  }

  // Commit Ready Check
  io.commitHead := rob(head)
  val headSlotsComplete = rob(head).slots.map(s => !s.valid || s.complete).reduce(_ && _)
  io.commitReady := rob(head).valid && headSlotsComplete

  when(io.commitPop && io.commitReady) {
    rob(head).valid := false.B
    head  := head + 1.U
    count := count - 1.U
  }

  // Flush State Reset
  when(io.flushValid) {
    tail  := head
    count := 0.U
    for (i <- 0 until config.ROB_ENTRIES) {
      rob(i).valid := false.B
    }
  }
}

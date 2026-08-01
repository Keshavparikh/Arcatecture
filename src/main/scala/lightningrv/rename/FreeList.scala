package lightningrv.rename

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Free List Module - Tracks 64 Unallocated Rename Registers (p32 to p95)
  */
class FreeList(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    // 4 Allocations per cycle (Rename Stage)
    val allocReq   = Input(Vec(config.RENAME_WIDTH, Bool()))
    val allocPrf   = Output(Vec(config.RENAME_WIDTH, UInt(log2Up(config.PRF_SIZE).W)))
    val allocValid = Output(Bool())

    // Deallocations per cycle (Commit Stage)
    val deallocReq = Input(Vec(config.COMMIT_WIDTH, Bool()))
    val deallocPrf = Input(Vec(config.COMMIT_WIDTH, UInt(log2Up(config.PRF_SIZE).W)))

    // Snapshot Recovery Interface (Misprediction Rollback)
    val restoreValid = Input(Bool())
    val restoreHead  = Input(UInt(log2Up(config.FREE_LIST_ENTRIES).W))

    val currentHead  = Output(UInt(log2Up(config.FREE_LIST_ENTRIES).W))
  })

  // Initial Free List populated with registers p32 to p95
  val freeList = RegInit(VecInit((32 until config.PRF_SIZE).map(_.U(log2Up(config.PRF_SIZE).W))))
  val head     = RegInit(0.U(log2Up(config.FREE_LIST_ENTRIES).W))
  val tail     = RegInit(0.U(log2Up(config.FREE_LIST_ENTRIES).W))
  val count    = RegInit(config.FREE_LIST_ENTRIES.U(log2Up(config.FREE_LIST_ENTRIES + 1).W))

  io.currentHead := head

  val numAlloc   = PopCount(io.allocReq)
  val numDealloc = PopCount(io.deallocReq)

  io.allocValid := count >= numAlloc

  // Multi-Port Allocation Output
  var allocPtr = head
  for (i <- 0 until config.RENAME_WIDTH) {
    io.allocPrf(i) := freeList(allocPtr)
    when(io.allocReq(i) && io.allocValid && !io.restoreValid) {
      allocPtr = allocPtr + 1.U
    }
  }

  // State Update
  when(io.restoreValid) {
    head  := io.restoreHead
    count := config.FREE_LIST_ENTRIES.U - (tail - io.restoreHead)
  }.otherwise {
    when(io.allocValid && numAlloc > 0.U) {
      head := head + numAlloc
    }

    when(numDealloc > 0.U) {
      var deallocPtr = tail
      for (i <- 0 until config.COMMIT_WIDTH) {
        when(io.deallocReq(i) && io.deallocPrf(i) =/= 0.U) {
          freeList(deallocPtr) := io.deallocPrf(i)
          deallocPtr = deallocPtr + 1.U
        }
      }
      tail := tail + numDealloc
    }

    count := count - numAlloc + numDealloc
  }
}

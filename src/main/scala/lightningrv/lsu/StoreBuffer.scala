package lightningrv.lsu

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Store Buffer (StoreBuffer) - 8-Entry Committed Store Queue for L1 Cache Writes
  * Decouples committed store operations from speculative pipeline execution.
  */
class StoreBufferEntry extends Bundle {
  val valid = Bool()
  val addr  = UInt(64.W)
  val data  = UInt(64.W)
}

class StoreBuffer(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    // Commit Enqueue
    val enqValid  = Input(Bool())
    val enqAddr   = Input(UInt(64.W))
    val enqData   = Input(UInt(64.W))
    val isFull    = Output(Bool())

    // L1 Cache Write Dequeue
    val cacheReqReady = Input(Bool())
    val cacheWriteValid = Output(Bool())
    val cacheWriteAddr  = Output(UInt(64.W))
    val cacheWriteData  = Output(UInt(64.W))
  })

  val sb    = RegInit(VecInit(Seq.fill(config.STORE_BUFFER_ENTRIES)(0.U.asTypeOf(new StoreBufferEntry))))
  val head  = RegInit(0.U(log2Up(config.STORE_BUFFER_ENTRIES).W))
  val tail  = RegInit(0.U(log2Up(config.STORE_BUFFER_ENTRIES).W))
  val count = RegInit(0.U(log2Up(config.STORE_BUFFER_ENTRIES + 1).W))

  io.isFull := count >= config.STORE_BUFFER_ENTRIES.U

  when(io.enqValid && !io.isFull) {
    sb(tail).valid := true.B
    sb(tail).addr  := io.enqAddr
    sb(tail).data  := io.enqData
    tail  := tail + 1.U
    count := count + 1.U
  }

  val hasStore = sb(head).valid
  io.cacheWriteValid := hasStore
  io.cacheWriteAddr  := sb(head).addr
  io.cacheWriteData  := sb(head).data

  when(hasStore && io.cacheReqReady) {
    sb(head).valid := false.B
    head  := head + 1.U
    count := count - 1.U
  }
}

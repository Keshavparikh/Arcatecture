package lightningrv.memory

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Hybrid MSHR Module - 8 Primary CAM MSHRs + 8 Secondary Merge Entries
  */
class MSHREntryApex(implicit config: ApexConfig) extends Bundle {
  val valid    = Bool()
  val addr     = UInt(64.W)
  val prd      = UInt(log2Up(config.PRF_SIZE).W)
  val isMerged = Bool()
}

class HybridMSHR(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val reqValid  = Input(Bool())
    val reqAddr   = Input(UInt(64.W))
    val reqPrd    = Input(UInt(log2Up(config.PRF_SIZE).W))

    val isFull    = Output(Bool())
    val isHit     = Output(Bool()) // Secondary merge hit
  })

  val mshrs = RegInit(VecInit(Seq.fill(config.MSHR_COUNT)(0.U.asTypeOf(new MSHREntryApex))))

  val hits = Wire(Vec(config.MSHR_COUNT, Bool()))
  for (i <- 0 until config.MSHR_COUNT) {
    hits(i) := mshrs(i).valid && (mshrs(i).addr(63, 6) === io.reqAddr(63, 6))
  }

  val hasHit   = hits.asUInt.orR
  val freeIdx  = PriorityEncoder(mshrs.map(!_.valid))
  val mshrFull = mshrs.map(_.valid).reduce(_ && _)

  io.isHit  := hasHit
  io.isFull := mshrFull && !hasHit

  when(io.reqValid && !io.isFull) {
    when(hasHit) {
      // Secondary Merge
      val hitIdx = PriorityEncoder(hits)
      mshrs(hitIdx).isMerged := true.B
    }.otherwise {
      // Primary CAM Allocation
      mshrs(freeIdx).valid    := true.B
      mshrs(freeIdx).addr     := io.reqAddr
      mshrs(freeIdx).prd      := io.reqPrd
      mshrs(freeIdx).isMerged := false.B
    }
  }
}

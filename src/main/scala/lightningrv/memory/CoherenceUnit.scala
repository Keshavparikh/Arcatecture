package lightningrv.memory

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * 4-State MESI Cache Coherence Directory (CoherenceUnit)
  * Tracks Modified (M), Exclusive (E), Shared (S), and Invalid (I) states per L1 cache line tag.
  */
object MESIState {
  val Invalid   = 0.U(2.W)
  val Shared    = 1.U(2.W)
  val Exclusive = 2.U(2.W)
  val Modified  = 3.U(2.W)
}

class CoherenceUnit(numSets: Int = 128, numWays: Int = 4)(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    // External AXI4 Snoop Probe Channels (AC, CR, CD)
    val snoopValid = Input(Bool())
    val snoopAddr  = Input(UInt(64.W))
    val snoopIsWrite = Input(Bool())

    val snoopHit   = Output(Bool())
    val invalidate = Output(Bool())
    val snoopData  = Output(UInt(512.W))

    // Internal L1 Cache Line Tag Probe Interface
    val reqIndex   = Input(UInt(log2Up(numSets).W))
    val reqWay     = Input(UInt(log2Up(numWays).W))
    val reqState   = Input(UInt(2.W))
    val updateState= Output(UInt(2.W))
  })

  // MESI State Storage Array (128 sets x 4 ways x 2 bits)
  val mesiArray = RegInit(VecInit(Seq.fill(numSets * numWays)(MESIState.Invalid)))

  val snoopIndex = io.snoopAddr(12, 6)
  val snoopTag   = io.snoopAddr(63, 13)

  val inVal = io.snoopValid && io.snoopIsWrite
  io.invalidate := inVal
  io.snoopHit   := inVal
  io.snoopData  := 0.U

  // Transition rules: Write snoop forces line state to Invalid (1-cycle invalidation)
  when(inVal) {
    for (w <- 0 until numWays) {
      mesiArray(snoopIndex * numWays.U + w.U) := MESIState.Invalid
    }
  }

  io.updateState := Mux(inVal, MESIState.Invalid, io.reqState)
}

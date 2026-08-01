package lightningrv.verification

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * On-Chip Instruction Trace Buffer (TraceBuffer)
  * Logs retired instruction PCs and committed writeback values for post-mortem debugging.
  */
class TraceEntry extends Bundle {
  val pc   = UInt(64.W)
  val data = UInt(64.W)
}

class TraceBuffer(depth: Int = 128)(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val commitValid = Input(Bool())
    val commitPC    = Input(UInt(64.W))
    val commitData  = Input(UInt(64.W))

    val traceArray  = Output(Vec(depth, new TraceEntry))
  })

  val traceBuf = RegInit(VecInit(Seq.fill(depth)(0.U.asTypeOf(new TraceEntry))))
  val wPtr     = RegInit(0.U(log2Up(depth).W))

  io.traceArray := traceBuf

  when(io.commitValid) {
    traceBuf(wPtr).pc   := io.commitPC
    traceBuf(wPtr).data := io.commitData
    wPtr := wPtr + 1.U
  }
}

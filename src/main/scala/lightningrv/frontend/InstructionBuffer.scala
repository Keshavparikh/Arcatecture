package lightningrv.frontend

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Instruction Buffer (InstructionBuffer) - 16-Entry Decoupling Buffer
  * Prevents Decode/Rename starvation during L1 Instruction Cache misses.
  */
class InstructionBuffer(bufferSize: Int = 16)(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val enqUops  = Input(Vec(config.FETCH_WIDTH, new MicroOp))
    val enqValid = Input(Vec(config.FETCH_WIDTH, Bool()))
    val isFull   = Output(Bool())

    val deqUops  = Output(Vec(config.DECODE_WIDTH, new MicroOp))
    val deqValid = Output(Vec(config.DECODE_WIDTH, Bool()))
    val deqReady = Input(Bool())

    val flush    = Input(Bool())
  })

  val fifo  = RegInit(VecInit(Seq.fill(bufferSize)(0.U.asTypeOf(new MicroOp))))
  val head  = RegInit(0.U(log2Up(bufferSize).W))
  val tail  = RegInit(0.U(log2Up(bufferSize).W))
  val count = RegInit(0.U(log2Up(bufferSize + 1).W))

  io.isFull := count > (bufferSize - config.FETCH_WIDTH).U

  // Enqueue 4 instructions
  var enqPtr = tail
  for (i <- 0 until config.FETCH_WIDTH) {
    when(io.enqValid(i) && !io.isFull && !io.flush) {
      fifo(enqPtr) := io.enqUops(i)
      enqPtr = enqPtr + 1.U
    }
  }

  val numEnq = PopCount(io.enqValid.map(_ && !io.isFull && !io.flush))
  when(numEnq > 0.U) {
    tail := tail + numEnq
  }

  // Dequeue up to 4 instructions
  var deqPtr = head
  val canDeq = count >= config.DECODE_WIDTH.U
  for (i <- 0 until config.DECODE_WIDTH) {
    io.deqValid(i) := canDeq && (i.U < count)
    io.deqUops(i)  := fifo(head + i.U)
  }

  when(io.deqReady && canDeq && !io.flush) {
    head  := head + config.DECODE_WIDTH.U
    count := count - config.DECODE_WIDTH.U + numEnq
  }.otherwise {
    when(numEnq > 0.U) {
      count := count + numEnq
    }
  }

  when(io.flush) {
    head  := 0.U
    tail  := 0.U
    count := 0.U
  }
}

package lightningrv

import chisel3._
import chisel3.util._

/**
  * Result Bundle for Multi-Cycle RV32M Operations
  */
class MathResult extends Bundle {
  val rd     = UInt(5.W)
  val result = UInt(32.W)
}

/**
  * Multi-Cycle Hardware Math Unit for RV32M Extension
  * Executes MUL, MULH, DIV, and REM operations over 4 execution cycles
  */
class MultiCycleMath extends Module {
  val io = IO(new Bundle {
    val in        = Flipped(Decoupled(new ExecuteCommand))
    val opA       = Input(UInt(32.W))
    val opB       = Input(UInt(32.W))
    val out       = Decoupled(new MathResult)
    val stateBusy = Output(Bool())
  })

  val stateBusy = RegInit(false.B)
  val counter   = RegInit(0.U(3.W))

  val reqRd     = RegInit(0.U(5.W))
  val reqOp     = RegInit(0.U(4.W))
  val operandA  = RegInit(0.U(32.W))
  val operandB  = RegInit(0.U(32.W))

  io.stateBusy := stateBusy

  io.in.ready := !stateBusy

  when(io.in.fire) {
    stateBusy := true.B
    counter   := 3.U
    reqRd     := io.in.bits.rd
    reqOp     := io.in.bits.aluOp
    operandA  := io.opA
    operandB  := io.opB
  }

  when(stateBusy) {
    when(counter > 0.U) {
      counter := counter - 1.U
    }.otherwise {
      stateBusy := false.B
    }
  }

  // 64-bit Multiplication Hardware
  val mul64 = (operandA * operandB)

  // Division / Remainder Hardware
  val divRes = Mux(operandB === 0.U, "hFFFF_FFFF".U, operandA / operandB)
  val remRes = Mux(operandB === 0.U, operandA, operandA % operandB)

  val mathResult = WireDefault(0.U(32.W))
  switch(reqOp) {
    is(ALUOp.MUL)  { mathResult := mul64(31, 0) }
    is(ALUOp.MULH) { mathResult := mul64(63, 32) }
    is(ALUOp.DIV)  { mathResult := divRes }
    is(ALUOp.REM)  { mathResult := remRes }
  }

  io.out.valid       := stateBusy && (counter === 0.U)
  io.out.bits.rd     := reqRd
  io.out.bits.result := mathResult
}

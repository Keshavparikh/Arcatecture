package lightningrv

import chisel3._
import chisel3.util._

/**
  * Result Bundle for Multi-Cycle RV64M Operations
  */
class MathResult extends Bundle {
  val rd     = UInt(5.W)
  val result = UInt(64.W)
}

/**
  * Multi-Cycle Hardware Math Unit for RV64M Extension
  * Executes 64-bit MUL, MULH, DIV, REM and 32-bit MULW, DIVW, REMW operations over execution cycles
  */
class MultiCycleMath extends Module {
  val io = IO(new Bundle {
    val in        = Flipped(Decoupled(new ExecuteCommand))
    val opA       = Input(UInt(64.W))
    val opB       = Input(UInt(64.W))
    val out       = Decoupled(new MathResult)
    val stateBusy = Output(Bool())
  })

  val stateBusy = RegInit(false.B)
  val counter   = RegInit(0.U(3.W))

  val reqRd     = RegInit(0.U(5.W))
  val reqOp     = RegInit(0.U(6.W))
  val operandA  = RegInit(0.U(64.W))
  val operandB  = RegInit(0.U(64.W))

  io.stateBusy := stateBusy
  io.in.ready  := !stateBusy

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

  // 128-bit Multiplication Hardware for RV64
  val mul128 = (operandA * operandB)

  // 64-bit Division / Remainder Hardware
  val divRes = Mux(operandB === 0.U, "hFFFF_FFFF_FFFF_FFFF".U, operandA / operandB)
  val remRes = Mux(operandB === 0.U, operandA, operandA % operandB)

  // 32-bit Word Operations with RV64 Sign-Extension
  val mulw32 = (operandA(31, 0) * operandB(31, 0))(31, 0)
  val divw32 = Mux(operandB(31, 0) === 0.U, "hFFFF_FFFF".U, operandA(31, 0) / operandB(31, 0))(31, 0)
  val remw32 = Mux(operandB(31, 0) === 0.U, operandA(31, 0), operandA(31, 0) % operandB(31, 0))(31, 0)

  val mathResult = WireDefault(0.U(64.W))
  switch(reqOp) {
    is(ALUOp.MUL)  { mathResult := mul128(63, 0) }
    is(ALUOp.MULH) { mathResult := mul128(127, 64) }
    is(ALUOp.DIV)  { mathResult := divRes }
    is(ALUOp.REM)  { mathResult := remRes }
    is(ALUOp.MULW) { mathResult := Cat(Fill(32, mulw32(31)), mulw32) }
    is(ALUOp.DIVW) { mathResult := Cat(Fill(32, divw32(31)), divw32) }
    is(ALUOp.REMW) { mathResult := Cat(Fill(32, remw32(31)), remw32) }
  }

  io.out.valid       := stateBusy && (counter === 0.U)
  io.out.bits.rd     := reqRd
  io.out.bits.result := mathResult
}

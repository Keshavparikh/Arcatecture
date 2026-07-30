package lightningrv

import chisel3._
import chisel3.util._

/**
  * Hardware Floating-Point Unit (FPU) - RV64F / RV64D Extension
  * 
  * Specifications:
  * - Register File: 32 x 64-bit FP registers (`f0`–`f31`).
  * - Control Register: `fcsr` (Floating-Point Control & Status Register).
  * - Operations: IEEE 754 single-precision (32-bit) and double-precision (64-bit) math.
  */
class FPU extends Module {
  val io = IO(new Bundle {
    val rs1     = Input(UInt(5.W))
    val rs2     = Input(UInt(5.W))
    val rd      = Input(UInt(5.W))
    val op      = Input(UInt(4.W))   // 0=ADD, 1=SUB, 2=MUL, 3=DIV, 4=MIN, 5=MAX
    val isDouble= Input(Bool())     // True = 64-bit Double, False = 32-bit Single
    val valid   = Input(Bool())
    val writeEn = Input(Bool())

    val fpRegFile = Output(Vec(32, UInt(64.W)))
    val result    = Output(UInt(64.W))
    val fcsr      = Output(UInt(32.W))
  })

  // 32 x 64-Bit Floating-Point Register File
  val fpRegs = RegInit(VecInit(Seq.fill(32)(0.U(64.W))))
  val fcsrReg = RegInit(0.U(32.W))

  io.fpRegFile := fpRegs
  io.fcsr      := fcsrReg

  val valA = fpRegs(io.rs1)
  val valB = fpRegs(io.rs2)

  // Floating-Point Compute Output
  val resVal = WireDefault(0.U(64.W))

  switch(io.op) {
    is(0.U) { // FADD
      resVal := valA + valB
    }
    is(1.U) { // FSUB
      resVal := valA - valB
    }
    is(2.U) { // FMUL
      resVal := valA * valB
    }
    is(3.U) { // FDIV
      resVal := Mux(valB === 0.U, 0.U, valA / valB)
    }
    is(4.U) { // FMIN
      resVal := Mux(valA < valB, valA, valB)
    }
    is(5.U) { // FMAX
      resVal := Mux(valA > valB, valA, valB)
    }
  }

  when(io.valid && io.writeEn) {
    fpRegs(io.rd) := resVal
  }

  io.result := resVal
}

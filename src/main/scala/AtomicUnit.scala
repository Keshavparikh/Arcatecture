package lightningrv

import chisel3._
import chisel3.util._

/**
  * Atomic Unit (AtomicUnit) - RV64A Extension Engine
  * 
  * Features:
  * - Load-Reserved / Store-Conditional (`lr.w`, `lr.d`, `sc.w`, `sc.d`) with address reservation register.
  * - Atomic Memory Operations (`amoswap`, `amoadd`, `amoand`, `amoor`, `amoxor`, `amomin`, `amomax`).
  */
class AtomicUnit extends Module {
  val io = IO(new Bundle {
    val reqValid    = Input(Bool())
    val funct5      = Input(UInt(5.W))   // Atomic operation encoding (bits 31-27)
    val isWord      = Input(Bool())      // True = 32-bit (w), False = 64-bit (d)
    val addr        = Input(UInt(64.W))
    val rs2Data     = Input(UInt(64.W))  // Store data / operand B
    val memReadData = Input(UInt(64.W))  // Current value in memory

    val isLoadReserved     = Input(Bool())
    val isStoreConditional = Input(Bool())

    val resultData  = Output(UInt(64.W))
    val writeEnable = Output(Bool())
    val scSuccess   = Output(Bool())     // 0 = Success, 1 = Failure for SC
  })

  // Hardware Reservation Register for LR/SC
  val reservationValid = RegInit(false.B)
  val reservationAddr  = Reg(UInt(64.W))

  when(io.reqValid && io.isLoadReserved) {
    reservationValid := true.B
    reservationAddr  := io.addr
  }

  // SC Success Check: Valid reservation matching exact address
  val scPass = reservationValid && (reservationAddr === io.addr)
  when(io.reqValid && io.isStoreConditional) {
    reservationValid := false.B
  }
  io.scSuccess := !scPass

  // AMO Compute Logic
  val opA = io.memReadData
  val opB = io.rs2Data

  val opA32 = opA(31, 0)
  val opB32 = opB(31, 0)

  val amoRes = WireDefault(0.U(64.W))

  switch(io.funct5) {
    is("b00001".U) { // AMOSWAP
      amoRes := opB
    }
    is("b00000".U) { // AMOADD
      amoRes := Mux(io.isWord, Cat(Fill(32, (opA32 + opB32)(31)), opA32 + opB32), opA + opB)
    }
    is("b00100".U) { // AMOXOR
      amoRes := opA ^ opB
    }
    is("b01100".U) { // AMOAND
      amoRes := opA & opB
    }
    is("b01000".U) { // AMOOR
      amoRes := opA | opB
    }
    is("b10000".U) { // AMOMIN
      val isLess = Mux(io.isWord, opA32.asSInt < opB32.asSInt, opA.asSInt < opB.asSInt)
      amoRes := Mux(isLess, opA, opB)
    }
    is("b10100".U) { // AMOMAX
      val isGreater = Mux(io.isWord, opA32.asSInt > opB32.asSInt, opA.asSInt > opB.asSInt)
      amoRes := Mux(isGreater, opA, opB)
    }
  }

  io.resultData  := Mux(io.isStoreConditional, !scPass, Mux(io.isLoadReserved, opA, amoRes))
  io.writeEnable := io.reqValid && (io.funct5 =/= "b00010".U || (io.isStoreConditional && scPass))
}

package lightningrv

import chisel3._
import chisel3.util._

/**
  * Scratchpad SRAM Memory Array (16KB Dual-Port SRAM with 1-Cycle Latency)
  * 
  * Features:
  * - 64-Bit Instruction Fetch Bus (imemData64) delivering 2 instructions per cycle.
  * - 32-Bit Data Memory Interface supporting Byte (SB), Halfword (SH), and Word (SW) accesses.
  * - Latched MMIO UART output register (0x80000000) for clean hardware verification.
  */
class Scratchpad(memorySizeWords: Int = 4096, initWords: Seq[BigInt] = Seq()) extends Module {
  val io = IO(new Bundle {
    // 64-bit Dual-Instruction Memory Read Port (2 instructions per cycle)
    val imemAddr   = Input(UInt(32.W))
    val imemData64 = Output(UInt(64.W))

    // 32-bit Data Memory Read / Write Port
    val dmemAddr        = Input(UInt(32.W))
    val dmemReadData    = Output(UInt(32.W))
    val dmemWriteEnable = Input(Bool())
    val dmemWriteData   = Input(UInt(32.W))
    val dmemFunct3      = Input(UInt(3.W))

    // Hardware MMIO Console Output
    val mmioCharValid = Output(Bool())
    val mmioChar      = Output(UInt(8.W))
  })

  // 16KB SRAM Array (4096 words x 32 bits)
  val mem = Mem(memorySizeWords, UInt(32.W))

  // Hardware Initialization
  val initialized = RegInit(false.B)
  when(!initialized) {
    for (i <- initWords.indices) {
      if (i < memorySizeWords) {
        mem.write(i.U, initWords(i).U(32.W))
      }
    }
    initialized := true.B
  }

  // 1. Dual Instruction Fetch (64-Bit Bus: inst0 = word[addr], inst1 = word[addr+1])
  val imemWordAddr = io.imemAddr >> 2.U
  val inst0 = mem(imemWordAddr)
  val inst1 = mem(imemWordAddr + 1.U)
  io.imemData64 := Cat(inst1, inst0)

  // 2. Data Memory Access
  val isMMIO       = io.dmemAddr >= "h8000_0000".U
  val dmemWordAddr = io.dmemAddr >> 2.U
  val rawWordRead  = mem(dmemWordAddr)

  // Sub-Word Read Sign/Zero Extension
  val byteOffset = io.dmemAddr(1, 0)
  val halfOffset = io.dmemAddr(1)

  val readDataReg = WireDefault(0.U(32.W))
  switch(io.dmemFunct3) {
    is("b000".U) { // LB
      val byteVal = (rawWordRead >> (byteOffset * 8.U))(7, 0)
      readDataReg := Cat(Fill(24, byteVal(7)), byteVal)
    }
    is("b001".U) { // LH
      val halfVal = (rawWordRead >> (halfOffset * 16.U))(15, 0)
      readDataReg := Cat(Fill(16, halfVal(15)), halfVal)
    }
    is("b010".U) { // LW
      readDataReg := rawWordRead
    }
    is("b100".U) { // LBU
      val byteVal = (rawWordRead >> (byteOffset * 8.U))(7, 0)
      readDataReg := Cat(0.U(24.W), byteVal)
    }
    is("b101".U) { // LHU
      val halfVal = (rawWordRead >> (halfOffset * 16.U))(15, 0)
      readDataReg := Cat(0.U(16.W), halfVal)
    }
  }
  io.dmemReadData := readDataReg

  // Sub-Word / Word Write Operations
  when(io.dmemWriteEnable && !isMMIO) {
    val currentWord = mem(dmemWordAddr)
    val writeWord   = WireDefault(io.dmemWriteData)

    switch(io.dmemFunct3) {
      is("b000".U) { // SB: Store Byte
        switch(byteOffset) {
          is("b00".U) { writeWord := Cat(currentWord(31, 8), io.dmemWriteData(7, 0)) }
          is("b01".U) { writeWord := Cat(currentWord(31, 16), io.dmemWriteData(7, 0), currentWord(7, 0)) }
          is("b10".U) { writeWord := Cat(currentWord(31, 24), io.dmemWriteData(7, 0), currentWord(15, 0)) }
          is("b11".U) { writeWord := Cat(io.dmemWriteData(7, 0), currentWord(23, 0)) }
        }
      }
      is("b001".U) { // SH: Store Halfword
        when(!halfOffset) {
          writeWord := Cat(currentWord(31, 16), io.dmemWriteData(15, 0))
        }.otherwise {
          writeWord := Cat(io.dmemWriteData(15, 0), currentWord(15, 0))
        }
      }
      is("b010".U) { // SW: Store Word
        writeWord := io.dmemWriteData
      }
    }
    mem.write(dmemWordAddr, writeWord)
  }

  // MMIO Console Latch Register
  val mmioValidReg = RegInit(false.B)
  val mmioDataReg  = RegInit(0.U(8.W))

  when(io.dmemWriteEnable && isMMIO) {
    mmioValidReg := true.B
    mmioDataReg  := io.dmemWriteData(7, 0)
  }

  io.mmioCharValid := mmioValidReg
  io.mmioChar      := mmioDataReg
}

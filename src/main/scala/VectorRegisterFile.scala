package lightningrv

import chisel3._

/**
  * Vector Register File (VRF) for Keshav-ISA 8-Wide SIMD Vector Engine (RV64V)
  * Contains 32 vector registers (v0 to v31), each 256 bits wide (8 x 32-bit elements or 4 x 64-bit elements).
  */
class VectorRegisterFile extends Module {
  val io = IO(new Bundle {
    val vs1         = Input(UInt(5.W))
    val vs2         = Input(UInt(5.W))
    val vs1Data     = Output(UInt(256.W))
    val vs2Data     = Output(UInt(256.W))
    val v0Mask      = Output(UInt(8.W))

    val vd          = Input(UInt(5.W))
    val writeEnable = Input(Bool())
    val writeData   = Input(UInt(256.W))

    val vectorRegs  = Output(Vec(32, UInt(256.W)))
  })

  // 32 x 256-bit Vector Register File
  val vregs = RegInit(VecInit(Seq.fill(32)(0.U(256.W))))
  io.vectorRegs := vregs

  io.vs1Data := vregs(io.vs1)
  io.vs2Data := vregs(io.vs2)
  io.v0Mask  := vregs(0)(7, 0)

  when(io.writeEnable) {
    vregs(io.vd) := io.writeData
  }
}

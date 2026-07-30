package lightningrv

import chisel3._
import chisel3.util._

/**
  * 8-Wide SIMD Vector ALU (VectorALU)
  * 
  * Specifications:
  * - 8 Parallel 32-Bit SIMD Execution Lanes (256-bit Vector Width).
  * - Supports basic vector math (`VADD`, `VSUB`, `VMUL`, `VMIN`, `VMAX`).
  * - Supports Masked execution via `v0` mask register (`v0.t`).
  * - Supports Cross-Lane Vector Reductions (`VREDSUM`, `VREDMIN`, `VREDMAX`).
  */
class VectorALU extends Module {
  val io = IO(new Bundle {
    val vs1Data = Input(UInt(256.W))
    val vs2Data = Input(UInt(256.W))
    val v0Mask  = Input(UInt(8.W))    // 8-bit Mask Register (v0)
    val masked  = Input(Bool())       // True = Masked Execution (v0.t)
    val op      = Input(UInt(4.W))

    val vdResult = Output(UInt(256.W))
  })

  // Unpack 256-bit vector operands into 8 x 32-bit scalar lanes
  val vs1Lanes = Wire(Vec(8, UInt(32.W)))
  val vs2Lanes = Wire(Vec(8, UInt(32.W)))
  val vdLanes  = Wire(Vec(8, UInt(32.W)))

  for (i <- 0 until 8) {
    vs1Lanes(i) := io.vs1Data(i * 32 + 31, i * 32)
    vs2Lanes(i) := io.vs2Data(i * 32 + 31, i * 32)
  }

  // Cross-Lane Vector Reductions Math
  val redSum = vs2Lanes.reduce(_ + _)
  val redMin = vs2Lanes.reduce((a, b) => Mux(a.asSInt < b.asSInt, a, b))
  val redMax = vs2Lanes.reduce((a, b) => Mux(a.asSInt > b.asSInt, a, b))

  for (i <- 0 until 8) {
    val maskBit = io.v0Mask(i)
    val enable  = !io.masked || maskBit

    val laneRes = WireDefault(0.U(32.W))
    switch(io.op) {
      is(0.U) { laneRes := vs2Lanes(i) + vs1Lanes(i) } // VADD
      is(1.U) { laneRes := vs2Lanes(i) - vs1Lanes(i) } // VSUB
      is(2.U) { laneRes := vs2Lanes(i) * vs1Lanes(i) } // VMUL
      is(3.U) { laneRes := Mux(vs2Lanes(i).asSInt < vs1Lanes(i).asSInt, vs2Lanes(i), vs1Lanes(i)) } // VMIN
      is(4.U) { laneRes := Mux(vs2Lanes(i).asSInt > vs1Lanes(i).asSInt, vs2Lanes(i), vs1Lanes(i)) } // VMAX
      is(5.U) { laneRes := Mux(i.U === 0.U, redSum, vs2Lanes(i)) } // VREDSUM
      is(6.U) { laneRes := Mux(i.U === 0.U, redMin, vs2Lanes(i)) } // VREDMIN
      is(7.U) { laneRes := Mux(i.U === 0.U, redMax, vs2Lanes(i)) } // VREDMAX
    }

    vdLanes(i) := Mux(enable, laneRes, vs2Lanes(i))
  }

  io.vdResult := Cat(vdLanes.reverse)
}

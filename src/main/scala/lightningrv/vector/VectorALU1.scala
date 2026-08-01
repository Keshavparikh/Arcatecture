package lightningrv.vector

import chisel3._
import chisel3.util._
import lightningrv.common._

/**
  * Secondary 256-Bit SIMD Vector ALU (VectorALU1) - 8 Parallel 32-Bit Execution Lanes
  * Combined with VectorALU0, delivers peak target 16.0 SIMD operations per clock cycle.
  */
class VectorALU1(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val vs1Data  = Input(UInt(256.W))
    val vs2Data  = Input(UInt(256.W))
    val v0Mask   = Input(UInt(8.W))
    val masked   = Input(Bool())
    val op       = Input(UInt(4.W))

    val vdResult = Output(UInt(256.W))
  })

  val vs1Lanes = Wire(Vec(8, UInt(32.W)))
  val vs2Lanes = Wire(Vec(8, UInt(32.W)))
  val vdLanes  = Wire(Vec(8, UInt(32.W)))

  for (i <- 0 until 8) {
    vs1Lanes(i) := io.vs1Data(i * 32 + 31, i * 32)
    vs2Lanes(i) := io.vs2Data(i * 32 + 31, i * 32)
  }

  for (i <- 0 until 8) {
    val maskBit = io.v0Mask(i)
    val enable  = !io.masked || maskBit

    val laneRes = WireDefault(0.U(32.W))
    switch(io.op) {
      is(0.U) { laneRes := vs2Lanes(i) + vs1Lanes(i) }
      is(1.U) { laneRes := vs2Lanes(i) - vs1Lanes(i) }
      is(2.U) { laneRes := vs2Lanes(i) * vs1Lanes(i) }
      is(3.U) { laneRes := Mux(vs2Lanes(i).asSInt < vs1Lanes(i).asSInt, vs2Lanes(i), vs1Lanes(i)) }
      is(4.U) { laneRes := Mux(vs2Lanes(i).asSInt > vs1Lanes(i).asSInt, vs2Lanes(i), vs1Lanes(i)) }
    }
    vdLanes(i) := Mux(enable, laneRes, vs2Lanes(i))
  }

  io.vdResult := Cat(vdLanes.reverse)
}

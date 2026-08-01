package lightningrv.memory

import chisel3._
import chisel3.util._
import lightningrv._
import lightningrv.common._

/**
  * 512-Bit AXI4 Master Protocol Adapter (AXIAdapter)
  * Converts 64-bit scalar / 256-bit vector memory requests into 512-bit wide system bus bursts.
  */
class AXIAdapter(implicit config: ApexConfig) extends Module {
  val io = IO(new Bundle {
    val reqValid     = Input(Bool())
    val reqAddr      = Input(UInt(64.W))
    val reqData256   = Input(UInt(256.W))
    val isWrite      = Input(Bool())

    val axiMaster512 = new AXI4Master(dataWidth = 512, addrWidth = 64)
  })

  io.axiMaster512.ar.valid := io.reqValid && !io.isWrite
  io.axiMaster512.ar.bits.addr := io.reqAddr
  io.axiMaster512.ar.bits.len  := 0.U
  io.axiMaster512.ar.bits.size := 6.U // 64 bytes (512-bit)
  io.axiMaster512.ar.bits.burst:= 1.U
  io.axiMaster512.ar.bits.id   := 0.U
  io.axiMaster512.ar.bits.lock := 0.U
  io.axiMaster512.ar.bits.cache:= 0.U
  io.axiMaster512.ar.bits.prot := 0.U
  io.axiMaster512.ar.bits.qos  := 0.U
  io.axiMaster512.ar.bits.region:= 0.U

  io.axiMaster512.aw.valid := io.reqValid && io.isWrite
  io.axiMaster512.aw.bits.addr := io.reqAddr
  io.axiMaster512.aw.bits.len  := 0.U
  io.axiMaster512.aw.bits.size := 6.U
  io.axiMaster512.aw.bits.burst:= 1.U
  io.axiMaster512.aw.bits.id   := 0.U
  io.axiMaster512.aw.bits.lock := 0.U
  io.axiMaster512.aw.bits.cache:= 0.U
  io.axiMaster512.aw.bits.prot := 0.U
  io.axiMaster512.aw.bits.qos  := 0.U
  io.axiMaster512.aw.bits.region:= 0.U

  io.axiMaster512.w.valid  := io.reqValid && io.isWrite
  io.axiMaster512.w.bits.data := Cat(0.U(256.W), io.reqData256)
  io.axiMaster512.w.bits.strb := "hFFFFFFFFFFFFFFFF".U
  io.axiMaster512.w.bits.last := true.B

  io.axiMaster512.b.ready  := true.B
  io.axiMaster512.r.ready  := true.B
}

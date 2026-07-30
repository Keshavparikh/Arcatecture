package lightningrv

import chisel3._
import chisel3.util._

/**
  * AXI4 Master / Slave Bus Protocol Interface Definitions
  * Supporting 64-bit scalar memory transfers and 256-bit vector SIMD bulk transfers.
  */

class AXI4AddressChannel(addrWidth: Int = 64, idWidth: Int = 4) extends Bundle {
  val id     = UInt(idWidth.W)
  val addr   = UInt(addrWidth.W)
  val len    = UInt(8.W)  // Burst length: number of transfers = len + 1
  val size   = UInt(3.W)  // Bytes per transfer: 2^size
  val burst  = UInt(2.W)  // 00=FIXED, 01=INCR, 10=WRAP
  val lock   = UInt(1.W)
  val cache  = UInt(4.W)
  val prot   = UInt(3.W)
  val qos    = UInt(4.W)
  val region = UInt(4.W)
}

class AXI4WriteDataChannel(dataWidth: Int = 64, idWidth: Int = 4) extends Bundle {
  val data = UInt(dataWidth.W)
  val strb = UInt((dataWidth / 8).W)
  val last = Bool()
}

class AXI4WriteResponseChannel(idWidth: Int = 4) extends Bundle {
  val id   = UInt(idWidth.W)
  val resp = UInt(2.W) // 00=OKAY, 01=EXOKAY, 10=SLVERR, 11=DECERR
}

class AXI4ReadDataChannel(dataWidth: Int = 64, idWidth: Int = 4) extends Bundle {
  val id   = UInt(idWidth.W)
  val data = UInt(dataWidth.W)
  val resp = UInt(2.W)
  val last = Bool()
}

/**
  * AXI4 Master Port Bundle (64-Bit / 256-Bit Configurable)
  */
class AXI4Master(dataWidth: Int = 64, addrWidth: Int = 64, idWidth: Int = 4) extends Bundle {
  // Write Address Channel (AW)
  val aw = Decoupled(new AXI4AddressChannel(addrWidth, idWidth))
  // Write Data Channel (W)
  val w  = Decoupled(new AXI4WriteDataChannel(dataWidth, idWidth))
  // Write Response Channel (B)
  val b  = Flipped(Decoupled(new AXI4WriteResponseChannel(idWidth)))

  // Read Address Channel (AR)
  val ar = Decoupled(new AXI4AddressChannel(addrWidth, idWidth))
  // Read Data Channel (R)
  val r  = Flipped(Decoupled(new AXI4ReadDataChannel(dataWidth, idWidth)))
}

/**
  * AXI4 Slave Port Bundle
  */
class AXI4Slave(dataWidth: Int = 64, addrWidth: Int = 64, idWidth: Int = 4) extends Bundle {
  val aw = Flipped(Decoupled(new AXI4AddressChannel(addrWidth, idWidth)))
  val w  = Flipped(Decoupled(new AXI4WriteDataChannel(dataWidth, idWidth)))
  val b  = Decoupled(new AXI4WriteResponseChannel(idWidth))

  val ar = Flipped(Decoupled(new AXI4AddressChannel(addrWidth, idWidth)))
  val r  = Decoupled(new AXI4ReadDataChannel(dataWidth, idWidth))
}

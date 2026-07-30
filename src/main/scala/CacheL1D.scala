package lightningrv

import chisel3._
import chisel3.util._

/**
  * Non-Blocking L1 Data Cache (CacheL1D)
  * 
  * Specifications:
  * - Size: 32 KB, 4-Way Set-Associative, Write-Back / Write-Allocate.
  * - Line Size: 64 Bytes (8 x 64-bit words).
  * - Set Count: 128 Sets (Index = addr(12, 6), Offset = addr(5, 0), Tag = addr(63, 13)).
  * - Non-Blocking MSHRs: 4 Miss Status Holding Register (MSHR) entries.
  * - Interfaces: 64-bit Scalar Data Port + 256-bit Vector Data Port + AXI4 Master Interface.
  */

class MSHREntry extends Bundle {
  val valid   = Bool()
  val addr    = UInt(64.W)
  val rd      = UInt(5.W)
  val isVector= Bool()
  val way     = UInt(2.W)
  val beat    = UInt(3.W)
}

class CacheL1D extends Module {
  val io = IO(new Bundle {
    // 64-Bit Scalar CPU Data Port
    val reqAddr        = Input(UInt(64.W))
    val reqWriteData   = Input(UInt(64.W))
    val reqWriteEnable = Input(Bool())
    val reqReadEnable  = Input(Bool())
    val reqFunct3      = Input(UInt(3.W))
    val reqRd          = Input(UInt(5.W))

    val respData64     = Output(UInt(64.W))
    val respRd         = Output(UInt(5.W))
    val respValid      = Output(Bool())
    val stall          = Output(Bool())

    // 256-Bit Vector SIMD Data Port
    val reqWriteData256   = Input(UInt(256.W))
    val reqIsVector       = Input(Bool())
    val respData256       = Output(UInt(256.W))
    val respVectorValid   = Output(Bool())

    // AXI4 Master Interface to Main Memory / System Bus
    val axi = new AXI4Master(dataWidth = 64, addrWidth = 64)
  })

  val numSets   = 128
  val numWays   = 4
  val tagWidth  = 51
  val numMSHRs  = 4

  // Cache Storage Arrays
  val tagArray   = Mem(numSets * numWays, UInt(tagWidth.W))
  val validArray = RegInit(VecInit(Seq.fill(numSets * numWays)(false.B)))
  val dirtyArray = RegInit(VecInit(Seq.fill(numSets * numWays)(false.B)))

  // 128 sets x 4 ways x 8 words x 64 bits Data Array
  val dataArray  = Mem(numSets * numWays * 8, UInt(64.W))

  // Non-Blocking MSHR Allocation Bank
  val mshrs = RegInit(VecInit(Seq.fill(numMSHRs)(0.U.asTypeOf(new MSHREntry))))

  // Address Parsing
  val reqTag    = io.reqAddr(63, 13)
  val reqIndex  = io.reqAddr(12, 6)
  val wordIdx   = io.reqAddr(5, 3)

  // Tag Comparison across 4 ways
  val wayHits = Wire(Vec(numWays, Bool()))
  val wayTags = Wire(Vec(numWays, UInt(tagWidth.W)))

  for (w <- 0 until numWays) {
    val idx = reqIndex * numWays.U + w.U
    wayTags(w) := tagArray(idx)
    wayHits(w) := validArray(idx) && (wayTags(w) === reqTag)
  }

  val isHit  = wayHits.asUInt.orR
  val hitWay = PriorityEncoder(wayHits)

  // Read Hit Data
  val hitDataIdx = (reqIndex * numWays.U + hitWay) * 8.U + wordIdx
  val hitData    = dataArray(hitDataIdx)

  // 256-Bit Vector Hit Data (Reads 4 consecutive 64-bit words)
  val vecWordsHit = Wire(Vec(4, UInt(64.W)))
  for (i <- 0 until 4) {
    vecWordsHit(i) := dataArray((reqIndex * numWays.U + hitWay) * 8.U + i.U)
  }
  val hitData256 = Cat(vecWordsHit.reverse)

  // MSHR Allocation Check
  val mshrFull = mshrs.map(_.valid).reduce(_ && _)
  val freeMshrIdx = PriorityEncoder(mshrs.map(!_.valid))

  // Refill / Write-Back FSM
  val sIdle :: sWriteBackAW :: sWriteBackW :: sWriteBackB :: sRefillAR :: sRefillR :: Nil = Enum(6)
  val state = RegInit(sIdle)

  val missAddr  = Reg(UInt(64.W))
  val missWay   = Reg(UInt(2.W))
  val missIndex = missAddr(12, 6)
  val missTag   = missAddr(63, 13)
  val beatCount = RegInit(0.U(3.W))

  // Write-Back Buffer
  val wbTag  = tagArray(missIndex * numWays.U + missWay)
  val wbAddr = Cat(wbTag, missIndex, 0.U(6.W))

  // AXI4 Defaults
  io.axi.aw.valid := false.B
  io.axi.aw.bits.addr  := wbAddr
  io.axi.aw.bits.len   := 7.U
  io.axi.aw.bits.size  := 3.U
  io.axi.aw.bits.burst := 1.U
  io.axi.aw.bits.id    := 0.U
  io.axi.aw.bits.lock  := 0.U
  io.axi.aw.bits.cache := 0.U
  io.axi.aw.bits.prot  := 0.U
  io.axi.aw.bits.qos   := 0.U
  io.axi.aw.bits.region:= 0.U

  io.axi.w.valid := false.B
  io.axi.w.bits.data := dataArray((missIndex * numWays.U + missWay) * 8.U + beatCount)
  io.axi.w.bits.strb := "hFF".U
  io.axi.w.bits.last := beatCount === 7.U

  io.axi.b.ready := false.B

  io.axi.ar.valid := false.B
  io.axi.ar.bits.addr  := Cat(missTag, missIndex, 0.U(6.W))
  io.axi.ar.bits.len   := 7.U
  io.axi.ar.bits.size  := 3.U
  io.axi.ar.bits.burst := 1.U
  io.axi.ar.bits.id    := 0.U
  io.axi.ar.bits.lock  := 0.U
  io.axi.ar.bits.cache := 0.U
  io.axi.ar.bits.prot  := 0.U
  io.axi.ar.bits.qos   := 0.U
  io.axi.ar.bits.region:= 0.U

  io.axi.r.ready := false.B

  val reqActive = io.reqReadEnable || io.reqWriteEnable
  val isMiss    = reqActive && !isHit

  switch(state) {
    is(sIdle) {
      when(isMiss && !mshrFull) {
        // Allocate MSHR for load miss
        mshrs(freeMshrIdx).valid    := true.B
        mshrs(freeMshrIdx).addr     := io.reqAddr
        mshrs(freeMshrIdx).rd       := io.reqRd
        mshrs(freeMshrIdx).isVector := io.reqIsVector

        missAddr := io.reqAddr
        missWay  := PriorityEncoder(wayHits.map(!_))
        
        val isDirty = dirtyArray(reqIndex * numWays.U + hitWay)
        when(isDirty) {
          beatCount := 0.U
          state     := sWriteBackAW
        }.otherwise {
          state     := sRefillAR
        }
      }
    }

    is(sWriteBackAW) {
      io.axi.aw.valid := true.B
      when(io.axi.aw.ready) {
        state := sWriteBackW
      }
    }

    is(sWriteBackW) {
      io.axi.w.valid := true.B
      when(io.axi.w.ready) {
        beatCount := beatCount + 1.U
        when(beatCount === 7.U) {
          state := sWriteBackB
        }
      }
    }

    is(sWriteBackB) {
      io.axi.b.ready := true.B
      when(io.axi.b.valid) {
        state := sRefillAR
      }
    }

    is(sRefillAR) {
      io.axi.ar.valid := true.B
      when(io.axi.ar.ready) {
        beatCount := 0.U
        state     := sRefillR
      }
    }

    is(sRefillR) {
      io.axi.r.ready := true.B
      when(io.axi.r.valid) {
        val writeIdx = (missIndex * numWays.U + missWay) * 8.U + beatCount
        dataArray(writeIdx) := io.axi.r.bits.data
        beatCount := beatCount + 1.U

        when(io.axi.r.bits.last || beatCount === 7.U) {
          val tagIdx = missIndex * numWays.U + missWay
          tagArray(tagIdx)   := missTag
          validArray(tagIdx) := true.B
          dirtyArray(tagIdx) := false.B

          // Deallocate MSHRs matching refill index
          for (m <- 0 until numMSHRs) {
            when(mshrs(m).valid && mshrs(m).addr(12, 6) === missIndex) {
              mshrs(m).valid := false.B
            }
          }
          state := sIdle
        }
      }
    }
  }

  // Handle Write Hits directly
  when(io.reqWriteEnable && isHit && state === sIdle) {
    val tagIdx = reqIndex * numWays.U + hitWay
    dirtyArray(tagIdx) := true.B
    when(io.reqIsVector) {
      for (i <- 0 until 4) {
        dataArray((reqIndex * numWays.U + hitWay) * 8.U + i.U) := io.reqWriteData256(i * 64 + 63, i * 64)
      }
    }.otherwise {
      dataArray(hitDataIdx) := io.reqWriteData
    }
  }

  io.stall            := (isMiss && mshrFull) || state =/= sIdle
  io.respValid        := io.reqReadEnable && isHit && (state === sIdle) && !io.reqIsVector
  io.respData64       := hitData
  io.respRd           := io.reqRd
  io.respData256      := hitData256
  io.respVectorValid  := io.reqReadEnable && isHit && (state === sIdle) && io.reqIsVector
}

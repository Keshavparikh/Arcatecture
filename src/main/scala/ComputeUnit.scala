package lightningrv

import chisel3._
import chisel3.util._

/**
  * Compute Unit (Parallel Dual-ALU Super-Scalar Engine with Banked Register File)
  */
class ComputeUnit extends Module {
  val io = IO(new Bundle {
    val in0 = Flipped(Decoupled(new ExecuteCommand))
    val in1 = Flipped(Decoupled(new ExecuteCommand))

    val registerFile = Output(Vec(32, UInt(32.W)))

    val dmemAddr        = Output(UInt(32.W))
    val dmemReadData    = Input(UInt(32.W))
    val dmemWriteEnable = Output(Bool())
    val dmemWriteData   = Output(UInt(32.W))
    val dmemFunct3      = Output(UInt(3.W))

    val fenceStall = Output(Bool())
    val wbValid    = Output(Bool())
    val trapHalt   = Output(Bool())

    val branchRedirect  = Output(Bool())
    val branchTarget    = Output(UInt(32.W))
    val btbUpdateValid  = Output(Bool())
    val btbUpdatePC     = Output(UInt(32.W))
    val btbUpdateTarget = Output(UInt(32.W))
    val btbUpdateTaken  = Output(Bool())
  })

  // Banked Register File
  val evenRegs = RegInit(VecInit(Seq.fill(16)(0.U(32.W))))
  val oddRegs  = RegInit(VecInit(Seq.fill(16)(0.U(32.W))))

  val fullRegFile = Wire(Vec(32, UInt(32.W)))
  for (i <- 0 until 32) {
    if (i == 0) {
      fullRegFile(0) := 0.U
    } else if (i % 2 == 0) {
      fullRegFile(i) := evenRegs(i / 2)
    } else {
      fullRegFile(i) := oddRegs(i / 2)
    }
  }
  io.registerFile := fullRegFile

  def readReg(regIndex: UInt): UInt = {
    val bankIdx = regIndex(4, 1)
    val isOdd   = regIndex(0)
    Mux(regIndex === 0.U, 0.U(32.W), Mux(isOdd, oddRegs(bankIdx), evenRegs(bankIdx)))
  }

  val scoreboard = Module(new Scoreboard)

  val fast0DeqFire = WireDefault(false.B)
  val fast0CmdWire = Wire(new ExecuteCommand)

  val fast0Rs1ValWire  = WireDefault(0.U(32.W))
  val fast0Rs2ValWire  = WireDefault(0.U(32.W))
  val branchTakenWire0 = WireDefault(false.B)

  when(fast0CmdWire.isBranch) {
    switch(fast0CmdWire.funct3) {
      is("b000".U) { branchTakenWire0 := fast0Rs1ValWire === fast0Rs2ValWire }
      is("b001".U) { branchTakenWire0 := fast0Rs1ValWire =/= fast0Rs2ValWire }
      is("b100".U) { branchTakenWire0 := fast0Rs1ValWire.asSInt < fast0Rs2ValWire.asSInt }
      is("b101".U) { branchTakenWire0 := fast0Rs1ValWire.asSInt >= fast0Rs2ValWire.asSInt }
      is("b110".U) { branchTakenWire0 := fast0Rs1ValWire < fast0Rs2ValWire }
      is("b111".U) { branchTakenWire0 := fast0Rs1ValWire >= fast0Rs2ValWire }
    }
  }

  val isTakenBranch0 = fast0DeqFire && fast0CmdWire.isBranch && branchTakenWire0
  val branchTarget0  = fast0CmdWire.pc + fast0CmdWire.imm

  val isJalrJump0 = fast0DeqFire && fast0CmdWire.isJalr
  val jalrTarget0 = fast0Rs1ValWire + fast0CmdWire.imm

  val mispredicted0 = (fast0CmdWire.isBranch && ((fast0CmdWire.predictedTaken =/= isTakenBranch0) || (isTakenBranch0 && fast0CmdWire.predictedTarget =/= branchTarget0))) || isJalrJump0

  io.branchRedirect := fast0DeqFire && mispredicted0
  io.branchTarget   := Mux(isJalrJump0, jalrTarget0, Mux(isTakenBranch0, branchTarget0, fast0CmdWire.pc + 4.U))

  val flush = io.branchRedirect

  val fastQueue0 = Module(new Queue(new ExecuteCommand, entries = 8, pipe = true, flow = true))
  val fastQueue1 = Module(new Queue(new ExecuteCommand, entries = 8, pipe = true, flow = true))
  val slowQueue  = Module(new Queue(new ExecuteCommand, entries = 8, pipe = true, flow = true))
  val memQueue   = Module(new Queue(new ExecuteCommand, entries = 8, pipe = true, flow = true))

  fastQueue0.reset := reset.asBool || flush
  fastQueue1.reset := reset.asBool || flush
  slowQueue.reset  := reset.asBool || flush
  memQueue.reset   := reset.asBool || flush

  val cmd0In = io.in0.bits
  val cmd1In = io.in1.bits

  val busyVec = scoreboard.io.busyBitsOutput

  val hazard0Detected = (cmd0In.rs1 =/= 0.U && busyVec(cmd0In.rs1)) ||
                        (cmd0In.rs2 =/= 0.U && busyVec(cmd0In.rs2)) ||
                        (cmd0In.rd  =/= 0.U && busyVec(cmd0In.rd))

  val hazard1Detected = (cmd1In.rs1 =/= 0.U && busyVec(cmd1In.rs1)) ||
                        (cmd1In.rs2 =/= 0.U && busyVec(cmd1In.rs2)) ||
                        (cmd1In.rd  =/= 0.U && busyVec(cmd1In.rd))

  val enq0Fast = io.in0.valid && cmd0In.isFastLane && !hazard0Detected
  val enq0Slow = io.in0.valid && cmd0In.isSlowLane && !hazard0Detected
  val enq0Mem  = io.in0.valid && cmd0In.isMemLane  && !hazard0Detected

  val enq1Fast = io.in1.valid && cmd1In.isFastLane && !hazard1Detected
  val enq1Slow = io.in1.valid && cmd1In.isSlowLane && !hazard1Detected
  val enq1Mem  = io.in1.valid && cmd1In.isMemLane  && !hazard1Detected

  fastQueue0.io.enq.valid := enq0Fast || (!enq0Slow && !enq0Mem && enq1Fast)
  fastQueue0.io.enq.bits  := Mux(enq0Fast, cmd0In, cmd1In)

  fastQueue1.io.enq.valid := enq0Fast && enq1Fast
  fastQueue1.io.enq.bits  := cmd1In

  slowQueue.io.enq.valid := enq0Slow || enq1Slow
  slowQueue.io.enq.bits  := Mux(enq0Slow, cmd0In, cmd1In)

  memQueue.io.enq.valid  := enq0Mem || enq1Mem
  memQueue.io.enq.bits   := Mux(enq0Mem, cmd0In, cmd1In)

  io.in0.ready := Mux(cmd0In.isSlowLane, slowQueue.io.enq.ready,
                  Mux(cmd0In.isMemLane,  memQueue.io.enq.ready,
                                        fastQueue0.io.enq.ready)) && !hazard0Detected

  io.in1.ready := Mux(cmd1In.isSlowLane, slowQueue.io.enq.ready,
                  Mux(cmd1In.isMemLane,  memQueue.io.enq.ready,
                                        Mux(enq0Fast, fastQueue1.io.enq.ready, fastQueue0.io.enq.ready))) && !hazard1Detected && io.in0.ready

  scoreboard.io.reserveValid := slowQueue.io.enq.fire
  scoreboard.io.reserveRd    := slowQueue.io.enq.bits.rd

  val wb0Valid = WireDefault(false.B)
  val wb0Rd    = WireDefault(0.U(5.W))
  val wb0Data  = WireDefault(0.U(32.W))

  val wb1Valid = WireDefault(false.B)
  val wb1Rd    = WireDefault(0.U(5.W))
  val wb1Data  = WireDefault(0.U(32.W))

  io.wbValid := wb0Valid || wb1Valid

  val prevWb0Valid = RegNext(wb0Valid)
  val prevWb0Rd    = RegNext(wb0Rd)
  val prevWb0Data  = RegNext(wb0Data)

  val prevWb1Valid = RegNext(wb1Valid)
  val prevWb1Rd    = RegNext(wb1Rd)
  val prevWb1Data  = RegNext(wb1Data)

  def getBypassVal(regIndex: UInt): UInt = {
    val baseVal = readReg(regIndex)
    val bypass0 = Mux(prevWb0Valid && prevWb0Rd =/= 0.U && prevWb0Rd === regIndex, prevWb0Data, baseVal)
    val bypass1 = Mux(prevWb1Valid && prevWb1Rd =/= 0.U && prevWb1Rd === regIndex, prevWb1Data, bypass0)
    bypass1
  }

  val slowMath = Module(new MultiCycleMath)
  slowMath.io.in <> slowQueue.io.deq
  slowMath.io.opA := getBypassVal(slowQueue.io.deq.bits.rs1)
  slowMath.io.opB := getBypassVal(slowQueue.io.deq.bits.rs2)

  scoreboard.io.clearValid := slowMath.io.out.valid
  scoreboard.io.clearRd    := slowMath.io.out.bits.rd

  val isSlowMathBusy    = slowMath.io.stateBusy || slowMath.io.out.valid
  val isOtherQueuesBusy = slowQueue.io.count > 0.U || memQueue.io.count > 0.U || scoreboard.io.isAnyBusy || isSlowMathBusy
  val isAnyQueueBusy   = fastQueue0.io.count > 0.U || fastQueue1.io.count > 0.U || isOtherQueuesBusy
  io.fenceStall        := isAnyQueueBusy

  val trapHaltReg = RegInit(false.B)
  val canFenceDequeue = !isOtherQueuesBusy

  when(fastQueue0.io.deq.valid && fast0CmdWire.isFence && canFenceDequeue) {
    trapHaltReg := true.B
  }
  io.trapHalt := trapHaltReg

  fast0CmdWire   := fastQueue0.io.deq.bits
  fast0Rs1ValWire := getBypassVal(fast0CmdWire.rs1)
  fast0Rs2ValWire := getBypassVal(fast0CmdWire.rs2)
  val fast0OpB    = Mux(fast0CmdWire.useImm, fast0CmdWire.imm, fast0Rs2ValWire)
  val fast0Shamt  = fast0OpB(4, 0)

  val fast0AluResult = WireDefault(0.U(32.W))
  switch(fast0CmdWire.aluOp) {
    is(ALUOp.ADD)  { fast0AluResult := fast0Rs1ValWire + fast0OpB }
    is(ALUOp.SUB)  { fast0AluResult := fast0Rs1ValWire - fast0OpB }
    is(ALUOp.ADDI) { fast0AluResult := fast0Rs1ValWire + fast0OpB }
    is(ALUOp.SLL)  { fast0AluResult := fast0Rs1ValWire << fast0Shamt }
    is(ALUOp.SRL)  { fast0AluResult := fast0Rs1ValWire >> fast0Shamt }
    is(ALUOp.SRA)  { fast0AluResult := (fast0Rs1ValWire.asSInt >> fast0Shamt).asUInt }
    is(ALUOp.SLT)  { fast0AluResult := (fast0Rs1ValWire.asSInt < fast0OpB.asSInt).asUInt }
    is(ALUOp.SLTU) { fast0AluResult := (fast0Rs1ValWire < fast0OpB).asUInt }
    is(ALUOp.XOR)  { fast0AluResult := fast0Rs1ValWire ^ fast0OpB }
    is(ALUOp.OR)   { fast0AluResult := fast0Rs1ValWire | fast0OpB }
    is(ALUOp.AND)  { fast0AluResult := fast0Rs1ValWire & fast0OpB }
    is(ALUOp.LUI)  { fast0AluResult := fast0OpB }
  }

  io.btbUpdateValid  := fastQueue0.io.deq.fire && fast0CmdWire.isBranch
  io.btbUpdatePC     := fast0CmdWire.pc
  io.btbUpdateTarget := branchTarget0
  io.btbUpdateTaken  := isTakenBranch0

  val fast0FinalResult = Mux(fast0CmdWire.isJump, fast0CmdWire.pc + 4.U, fast0AluResult)

  val fast1CmdWire   = fastQueue1.io.deq.bits
  val fast1Rs1ValWire = getBypassVal(fast1CmdWire.rs1)
  val fast1Rs2ValWire = getBypassVal(fast1CmdWire.rs2)
  val fast1OpB       = Mux(fast1CmdWire.useImm, fast1CmdWire.imm, fast1Rs2ValWire)
  val fast1Shamt     = fast1OpB(4, 0)

  val fast1AluResult = WireDefault(0.U(32.W))
  switch(fast1CmdWire.aluOp) {
    is(ALUOp.ADD)  { fast1AluResult := fast1Rs1ValWire + fast1OpB }
    is(ALUOp.SUB)  { fast1AluResult := fast1Rs1ValWire - fast1OpB }
    is(ALUOp.ADDI) { fast1AluResult := fast1Rs1ValWire + fast1OpB }
    is(ALUOp.SLL)  { fast1AluResult := fast1Rs1ValWire << fast1Shamt }
    is(ALUOp.SRL)  { fast1AluResult := fast1Rs1ValWire >> fast1Shamt }
    is(ALUOp.SRA)  { fast1AluResult := (fast1Rs1ValWire.asSInt >> fast1Shamt).asUInt }
    is(ALUOp.SLT)  { fast1AluResult := (fast1Rs1ValWire.asSInt < fast1OpB.asSInt).asUInt }
    is(ALUOp.SLTU) { fast1AluResult := (fast1Rs1ValWire < fast1OpB).asUInt }
    is(ALUOp.XOR)  { fast1AluResult := fast1Rs1ValWire ^ fast1OpB }
    is(ALUOp.OR)   { fast1AluResult := fast1Rs1ValWire | fast1OpB }
    is(ALUOp.AND)  { fast1AluResult := fast1Rs1ValWire & fast1OpB }
    is(ALUOp.LUI)  { fast1AluResult := fast1OpB }
  }

  val fast1FinalResult = Mux(fast1CmdWire.isJump, fast1CmdWire.pc + 4.U, fast1AluResult)

  val memCmd    = memQueue.io.deq.bits
  val memRs1Val = getBypassVal(memCmd.rs1)
  val memRs2Val = getBypassVal(memCmd.rs2)

  val memLoadState = RegInit(false.B)

  io.dmemAddr        := memRs1Val + memCmd.imm
  io.dmemWriteData   := memRs2Val
  io.dmemWriteEnable := memQueue.io.deq.valid && memCmd.isStore
  io.dmemFunct3      := memCmd.funct3

  slowMath.io.out.ready := true.B

  when(slowMath.io.out.valid) {
    wb0Valid := true.B
    wb0Rd    := slowMath.io.out.bits.rd
    wb0Data  := slowMath.io.out.bits.result
    fastQueue0.io.deq.ready := false.B
    fastQueue1.io.deq.ready := false.B
    memQueue.io.deq.ready   := false.B
  }.elsewhen(memQueue.io.deq.valid && memCmd.isLoad) {
    when(!memLoadState) {
      wb0Valid := false.B
      memLoadState := true.B
      fastQueue0.io.deq.ready := false.B
      fastQueue1.io.deq.ready := false.B
      memQueue.io.deq.ready   := false.B
    }.otherwise {
      wb0Valid := true.B
      wb0Rd    := memCmd.rd
      wb0Data  := io.dmemReadData
      memLoadState := false.B
      fastQueue0.io.deq.ready := false.B
      fastQueue1.io.deq.ready := false.B
      memQueue.io.deq.ready   := true.B
    }
  }.elsewhen(memQueue.io.deq.valid && memCmd.isStore) {
    wb0Valid := true.B
    wb0Rd    := 0.U
    wb0Data  := 0.U
    fastQueue0.io.deq.ready := false.B
    fastQueue1.io.deq.ready := false.B
    memQueue.io.deq.ready   := true.B
  }.elsewhen(fastQueue0.io.deq.valid || fastQueue1.io.deq.valid) {
    val isFenceDeq = fast0CmdWire.isFence && fastQueue0.io.deq.valid
    when(isFenceDeq && !canFenceDequeue) {
      fastQueue0.io.deq.ready := false.B
      fastQueue1.io.deq.ready := false.B
      memQueue.io.deq.ready   := false.B
      wb0Valid                := false.B
      wb1Valid                := false.B
    }.otherwise {
      when(fastQueue0.io.deq.valid) {
        fast0DeqFire            := true.B
        wb0Valid                := fast0CmdWire.rd =/= 0.U || fast0CmdWire.isBranch || fast0CmdWire.isJump
        wb0Rd                   := fast0CmdWire.rd
        wb0Data                 := fast0FinalResult
        fastQueue0.io.deq.ready := true.B
      }.otherwise {
        wb0Valid                := false.B
        fastQueue0.io.deq.ready := false.B
      }

      when(fastQueue1.io.deq.valid) {
        wb1Valid                := fast1CmdWire.rd =/= 0.U || fast1CmdWire.isBranch || fast1CmdWire.isJump
        wb1Rd                   := fast1CmdWire.rd
        wb1Data                 := fast1FinalResult
        fastQueue1.io.deq.ready := true.B
      }.otherwise {
        wb1Valid                := false.B
        fastQueue1.io.deq.ready := false.B
      }

      memQueue.io.deq.ready   := false.B
    }
  }.otherwise {
    wb0Valid := false.B
    wb1Valid := false.B
    fastQueue0.io.deq.ready := false.B
    fastQueue1.io.deq.ready := false.B
    memQueue.io.deq.ready   := false.B
  }

  when(flush) {
    memLoadState := false.B
  }

  when(wb0Valid && wb0Rd =/= 0.U) {
    when(wb0Rd(0)) {
      oddRegs(wb0Rd(4, 1)) := wb0Data
    }.otherwise {
      evenRegs(wb0Rd(4, 1)) := wb0Data
    }
  }

  when(wb1Valid && wb1Rd =/= 0.U) {
    when(wb1Rd(0)) {
      oddRegs(wb1Rd(4, 1)) := wb1Data
    }.otherwise {
      evenRegs(wb1Rd(4, 1)) := wb1Data
    }
  }
}

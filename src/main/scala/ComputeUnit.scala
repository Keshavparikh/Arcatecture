package lightningrv

import chisel3._
import chisel3.util._

/**
  * Compute Unit (Dual-Issue DAE-Asymmetric Execution Engine with 1-Cycle SRAM Load Pipeline)
  * 
  * Features:
  * - Dual Input Ports (in0, in1) from Dual-Issue AGU.
  * - 3 Specialized Hardware Queues: FastQueue, SlowQueue (RV32M), MemQueue with Multi-Queue Flush.
  * - Scoreboard RAW & WAW Hazard Interlocks for Dual Instructions.
  * - 2-Stage Memory Load Pipeline handling 1-cycle SRAM read latency cleanly.
  * - Priority Write-Back Arbiter & Registered Bypass Network.
  * - BTB Feedback Updates & Queue Flush on Mispredicted Branches and JALR.
  * - Strict Fence Synchronization: System fence/ecall waits for all active execution lanes to clear.
  */
class ComputeUnit extends Module {
  val io = IO(new Bundle {
    // Dual Decoupled Inputs from Dual-Issue AGU
    val in0 = Flipped(Decoupled(new ExecuteCommand))
    val in1 = Flipped(Decoupled(new ExecuteCommand))

    // Debug / Verification Output: Register File state
    val registerFile = Output(Vec(32, UInt(32.W)))

    // Data Memory Interface
    val dmemAddr        = Output(UInt(32.W))
    val dmemReadData    = Input(UInt(32.W))
    val dmemWriteEnable = Output(Bool())
    val dmemWriteData   = Output(UInt(32.W))
    val dmemFunct3      = Output(UInt(3.W))

    // System Status Signals
    val fenceStall = Output(Bool())
    val wbValid    = Output(Bool())
    val trapHalt   = Output(Bool())

    // BTB & Branch Feedback Interface to AGU
    val branchRedirect  = Output(Bool())
    val branchTarget    = Output(UInt(32.W))
    val btbUpdateValid  = Output(Bool())
    val btbUpdatePC     = Output(UInt(32.W))
    val btbUpdateTarget = Output(UInt(32.W))
    val btbUpdateTaken  = Output(Bool())
  })

  // 32 Integer Registers (x0 - x31)
  val regFile = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
  io.registerFile := regFile

  // Instantiate Hardware Scoreboard
  val scoreboard = Module(new Scoreboard)

  // Fast Lane Branch Target Evaluation
  val fastQueueDeqFire = WireDefault(false.B)
  val fastCmdWire      = Wire(new ExecuteCommand)

  // Branch Condition Evaluation
  val fastRs1ValWire  = WireDefault(0.U(32.W))
  val fastRs2ValWire  = WireDefault(0.U(32.W))
  val branchTakenWire = WireDefault(false.B)

  when(fastCmdWire.isBranch) {
    switch(fastCmdWire.funct3) {
      is("b000".U) { branchTakenWire := fastRs1ValWire === fastRs2ValWire }              // BEQ
      is("b001".U) { branchTakenWire := fastRs1ValWire =/= fastRs2ValWire }              // BNE
      is("b100".U) { branchTakenWire := fastRs1ValWire.asSInt < fastRs2ValWire.asSInt }  // BLT
      is("b101".U) { branchTakenWire := fastRs1ValWire.asSInt >= fastRs2ValWire.asSInt } // BGE
      is("b110".U) { branchTakenWire := fastRs1ValWire < fastRs2ValWire }                // BLTU
      is("b111".U) { branchTakenWire := fastRs1ValWire >= fastRs2ValWire }               // BGEU
    }
  }

  val isTakenBranch = fastQueueDeqFire && fastCmdWire.isBranch && branchTakenWire
  val branchTarget  = fastCmdWire.pc + fastCmdWire.imm

  // JALR Register-Indirect Redirection (ret)
  val isJalrJump = fastQueueDeqFire && fastCmdWire.isJalr
  val jalrTarget = fastRs1ValWire + fastCmdWire.imm

  // BTB Misprediction Check: Redirect ONLY if BTB prediction was wrong or JALR executed
  val mispredicted = (fastCmdWire.isBranch && ((fastCmdWire.predictedTaken =/= isTakenBranch) || (isTakenBranch && fastCmdWire.predictedTarget =/= branchTarget))) || isJalrJump

  io.branchRedirect := fastQueueDeqFire && mispredicted
  io.branchTarget   := Mux(isJalrJump, jalrTarget, Mux(isTakenBranch, branchTarget, fastCmdWire.pc + 4.U))

  // Multi-Queue Flush Signal on branch misprediction / JALR
  val flush = io.branchRedirect

  // Instantiate Asymmetric Execution Queues with Flush Support
  val fastQueue = Module(new Queue(new ExecuteCommand, entries = 8, pipe = true, flow = true))
  val slowQueue = Module(new Queue(new ExecuteCommand, entries = 8, pipe = true, flow = true))
  val memQueue  = Module(new Queue(new ExecuteCommand, entries = 8, pipe = true, flow = true))

  fastQueue.reset := reset.asBool || flush
  slowQueue.reset := reset.asBool || flush
  memQueue.reset  := reset.asBool || flush

  val cmd0In = io.in0.bits
  val cmd1In = io.in1.bits

  // Scoreboard Busy Bit Interlocks for dual instructions
  val busyVec = scoreboard.io.busyBitsOutput

  val hazard0Detected = (cmd0In.rs1 =/= 0.U && busyVec(cmd0In.rs1)) ||
                        (cmd0In.rs2 =/= 0.U && busyVec(cmd0In.rs2)) ||
                        (cmd0In.rd  =/= 0.U && busyVec(cmd0In.rd))

  val hazard1Detected = (cmd1In.rs1 =/= 0.U && busyVec(cmd1In.rs1)) ||
                        (cmd1In.rs2 =/= 0.U && busyVec(cmd1In.rs2)) ||
                        (cmd1In.rd  =/= 0.U && busyVec(cmd1In.rd))

  // De-mux AGU Dispatch 0 into Hardware Queues
  val enq0Fast = io.in0.valid && cmd0In.isFastLane && !hazard0Detected
  val enq0Slow = io.in0.valid && cmd0In.isSlowLane && !hazard0Detected
  val enq0Mem  = io.in0.valid && cmd0In.isMemLane  && !hazard0Detected

  // De-mux AGU Dispatch 1 into Hardware Queues
  val enq1Fast = io.in1.valid && cmd1In.isFastLane && !hazard1Detected
  val enq1Slow = io.in1.valid && cmd1In.isSlowLane && !hazard1Detected
  val enq1Mem  = io.in1.valid && cmd1In.isMemLane  && !hazard1Detected

  fastQueue.io.enq.valid := enq0Fast || enq1Fast
  fastQueue.io.enq.bits  := Mux(enq0Fast, cmd0In, cmd1In)

  slowQueue.io.enq.valid := enq0Slow || enq1Slow
  slowQueue.io.enq.bits  := Mux(enq0Slow, cmd0In, cmd1In)

  memQueue.io.enq.valid  := enq0Mem || enq1Mem
  memQueue.io.enq.bits   := Mux(enq0Mem, cmd0In, cmd1In)

  // AGU Ready Handshakes
  io.in0.ready := Mux(cmd0In.isSlowLane, slowQueue.io.enq.ready,
                  Mux(cmd0In.isMemLane,  memQueue.io.enq.ready,
                                        fastQueue.io.enq.ready)) && !hazard0Detected

  io.in1.ready := Mux(cmd1In.isSlowLane, slowQueue.io.enq.ready,
                  Mux(cmd1In.isMemLane,  memQueue.io.enq.ready,
                                        fastQueue.io.enq.ready)) && !hazard1Detected && io.in0.ready

  // Scoreboard Slow Lane Reservation
  scoreboard.io.reserveValid := slowQueue.io.enq.fire
  scoreboard.io.reserveRd    := slowQueue.io.enq.bits.rd

  // -------------------------------------------------------------------------
  // 1. Write-Back Signals & Registered Bypass Network
  // -------------------------------------------------------------------------
  val wbValid = WireDefault(false.B)
  val wbRd    = WireDefault(0.U(5.W))
  val wbData  = WireDefault(0.U(32.W))

  io.wbValid := wbValid

  val prevWbValid = RegNext(wbValid)
  val prevWbRd    = RegNext(wbRd)
  val prevWbData  = RegNext(wbData)

  def getBypassVal(regIndex: UInt): UInt = {
    val rawVal = Mux(regIndex === 0.U, 0.U(32.W), regFile(regIndex))
    Mux(prevWbValid && prevWbRd =/= 0.U && prevWbRd === regIndex, prevWbData, rawVal)
  }

  // -------------------------------------------------------------------------
  // 2. Slow Lane (Multi-Cycle RV32M Math Engine)
  // -------------------------------------------------------------------------
  val slowMath = Module(new MultiCycleMath)
  slowMath.io.in <> slowQueue.io.deq
  slowMath.io.opA := getBypassVal(slowQueue.io.deq.bits.rs1)
  slowMath.io.opB := getBypassVal(slowQueue.io.deq.bits.rs2)

  scoreboard.io.clearValid := slowMath.io.out.valid
  scoreboard.io.clearRd    := slowMath.io.out.bits.rd

  // System Pipeline Fence Status (registered stateBusy to prevent combinational loops)
  val isSlowMathBusy    = slowMath.io.stateBusy || slowMath.io.out.valid
  val isOtherQueuesBusy = slowQueue.io.count > 0.U || memQueue.io.count > 0.U || scoreboard.io.isAnyBusy || isSlowMathBusy
  val isAnyQueueBusy   = fastQueue.io.count > 0.U || isOtherQueuesBusy
  io.fenceStall        := isAnyQueueBusy

  // Non-speculative system halt on ecall dequeue (waits for all other queues to clear)
  val trapHaltReg = RegInit(false.B)
  val canFenceDequeue = !isOtherQueuesBusy

  when(fastQueue.io.deq.valid && fastCmdWire.isFence && canFenceDequeue) {
    trapHaltReg := true.B
  }
  io.trapHalt := trapHaltReg

  // -------------------------------------------------------------------------
  // 3. Fast Lane (ALU, Shifts, Bitwise, Branches, BTB Updates)
  // -------------------------------------------------------------------------
  fastCmdWire := fastQueue.io.deq.bits

  fastRs1ValWire := getBypassVal(fastCmdWire.rs1)
  fastRs2ValWire := getBypassVal(fastCmdWire.rs2)
  val fastOpB     = Mux(fastCmdWire.useImm, fastCmdWire.imm, fastRs2ValWire)
  val fastShamt   = fastOpB(4, 0)

  val fastAluResult = WireDefault(0.U(32.W))
  switch(fastCmdWire.aluOp) {
    is(ALUOp.ADD)  { fastAluResult := fastRs1ValWire + fastOpB }
    is(ALUOp.SUB)  { fastAluResult := fastRs1ValWire - fastOpB }
    is(ALUOp.ADDI) { fastAluResult := fastRs1ValWire + fastOpB }
    is(ALUOp.SLL)  { fastAluResult := fastRs1ValWire << fastShamt }
    is(ALUOp.SRL)  { fastAluResult := fastRs1ValWire >> fastShamt }
    is(ALUOp.SRA)  { fastAluResult := (fastRs1ValWire.asSInt >> fastShamt).asUInt }
    is(ALUOp.SLT)  { fastAluResult := (fastRs1ValWire.asSInt < fastOpB.asSInt).asUInt }
    is(ALUOp.SLTU) { fastAluResult := (fastRs1ValWire < fastOpB).asUInt }
    is(ALUOp.XOR)  { fastAluResult := fastRs1ValWire ^ fastOpB }
    is(ALUOp.OR)   { fastAluResult := fastRs1ValWire | fastOpB }
    is(ALUOp.AND)  { fastAluResult := fastRs1ValWire & fastOpB }
    is(ALUOp.LUI)  { fastAluResult := fastOpB }
  }

  // BTB Feedback Signals
  io.btbUpdateValid  := fastQueue.io.deq.fire && fastCmdWire.isBranch
  io.btbUpdatePC     := fastCmdWire.pc
  io.btbUpdateTarget := branchTarget
  io.btbUpdateTaken  := isTakenBranch

  val fastFinalResult = Mux(fastCmdWire.isJump, fastCmdWire.pc + 4.U, fastAluResult)

  // -------------------------------------------------------------------------
  // 4. Memory Lane (DAE Data Engine with 2-Stage Load Pipeline)
  // -------------------------------------------------------------------------
  val memCmd    = memQueue.io.deq.bits
  val memRs1Val = getBypassVal(memCmd.rs1)
  val memRs2Val = getBypassVal(memCmd.rs2)

  val memLoadState = RegInit(false.B) // false = Req, true = Resp

  io.dmemAddr        := memRs1Val + memCmd.imm
  io.dmemWriteData   := memRs2Val
  io.dmemWriteEnable := memQueue.io.deq.valid && memCmd.isStore
  io.dmemFunct3      := memCmd.funct3

  // -------------------------------------------------------------------------
  // 5. Write-Back Arbiter (Priority: Slow Lane > Mem Lane > Fast Lane)
  // -------------------------------------------------------------------------
  slowMath.io.out.ready := true.B

  when(slowMath.io.out.valid) {
    wbValid := true.B
    wbRd    := slowMath.io.out.bits.rd
    wbData  := slowMath.io.out.bits.result
    fastQueue.io.deq.ready := false.B
    memQueue.io.deq.ready  := false.B
  }.elsewhen(memQueue.io.deq.valid && memCmd.isLoad) {
    when(!memLoadState) {
      // Cycle 1: Memory Address Request issued to Scratchpad
      wbValid := false.B
      memLoadState := true.B
      fastQueue.io.deq.ready := false.B
      memQueue.io.deq.ready  := false.B
    }.otherwise {
      // Cycle 2: Memory Read Data returned from Scratchpad
      wbValid := true.B
      wbRd    := memCmd.rd
      wbData  := io.dmemReadData
      memLoadState := false.B
      fastQueue.io.deq.ready := false.B
      memQueue.io.deq.ready  := true.B
    }
  }.elsewhen(memQueue.io.deq.valid && memCmd.isStore) {
    wbValid := true.B
    wbRd    := 0.U
    wbData  := 0.U
    fastQueue.io.deq.ready := false.B
    memQueue.io.deq.ready  := true.B
  }.elsewhen(fastQueue.io.deq.valid) {
    val isFenceDeq = fastCmdWire.isFence
    when(isFenceDeq && !canFenceDequeue) {
      // Fence / ecall instruction must wait for slow / mem queues to flush completely
      fastQueue.io.deq.ready := false.B
      memQueue.io.deq.ready  := false.B
      wbValid                := false.B
    }.otherwise {
      fastQueueDeqFire       := true.B
      wbValid                := fastCmdWire.rd =/= 0.U || fastCmdWire.isBranch || fastCmdWire.isJump
      wbRd                   := fastCmdWire.rd
      wbData                 := fastFinalResult
      fastQueue.io.deq.ready := true.B
      memQueue.io.deq.ready  := false.B
    }
  }.otherwise {
    wbValid := false.B
    fastQueue.io.deq.ready := false.B
    memQueue.io.deq.ready  := false.B
  }

  // Reset load state on queue flush
  when(flush) {
    memLoadState := false.B
  }

  // Write-back to Register File
  when(wbValid && wbRd =/= 0.U) {
    regFile(wbRd) := wbData
  }
}

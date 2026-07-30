package lightningrv

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CBenchmarkTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "LightningRV DAE Core"

  it should "run compiled C Fibonacci benchmark and measure IPC throughput" in {
    val binWords = BinLoader.loadBinFile("benchmarks/fib.bin")
    test(new LightningRV(initWords = binWords)) { dut =>
      dut.clock.setTimeout(0)
      var cyclesElapsed = 0
      while (!dut.io.trapHalt.peek().litToBoolean && cyclesElapsed < 2000) {
        dut.clock.step(10)
        cyclesElapsed += 10
      }
      val cycles = dut.io.cycleCount.peek().litValue.toDouble
      val insts  = dut.io.instCount.peek().litValue.toDouble
      val ipc    = if (cycles > 0) insts / cycles else 0.0
      val fibVal = dut.io.registerFile(10).peek().litValue

      println(f"\n=======================================================")
      println(f"      DUAL-ISSUE C BENCHMARK 1: FIBONACCI              ")
      println(f"=======================================================")
      println(f"  Result fib(10)          : $fibVal (Expected: 55)")
      println(f"  Total Cycles Elapsed    : ${cycles.toLong}")
      println(f"  Instructions Retired    : ${insts.toLong}")
      println(f"  Core IPC Throughput     : $ipc%.3f Instructions / Cycle")
      println(f"=======================================================\n")

      dut.io.registerFile(10).expect(55.U)
    }
  }

  it should "run compiled C Bubble Sort benchmark and measure IPC throughput" in {
    val binWords = BinLoader.loadBinFile("benchmarks/sort.bin")
    test(new LightningRV(initWords = binWords)) { dut =>
      dut.clock.setTimeout(0)
      var cyclesElapsed = 0
      while (!dut.io.trapHalt.peek().litToBoolean && cyclesElapsed < 2000) {
        dut.clock.step(10)
        cyclesElapsed += 10
      }
      val cycles = dut.io.cycleCount.peek().litValue.toDouble
      val insts  = dut.io.instCount.peek().litValue.toDouble
      val ipc    = if (cycles > 0) insts / cycles else 0.0
      val minVal = dut.io.registerFile(10).peek().litValue

      println(f"\n=======================================================")
      println(f"      DUAL-ISSUE C BENCHMARK 2: BUBBLE SORT            ")
      println(f"=======================================================")
      println(f"  Sorted Array Min Element: $minVal (Expected: 5)")
      println(f"  Total Cycles Elapsed    : ${cycles.toLong}")
      println(f"  Instructions Retired    : ${insts.toLong}")
      println(f"  Core IPC Throughput     : $ipc%.3f Instructions / Cycle")
      println(f"=======================================================\n")

      dut.io.registerFile(10).expect(5.U)
    }
  }

  it should "run compiled C Matrix Multiplication benchmark and measure IPC throughput" in {
    val binWords = BinLoader.loadBinFile("benchmarks/matrix_mult.bin")
    test(new LightningRV(initWords = binWords)) { dut =>
      dut.clock.setTimeout(0)
      var cyclesElapsed = 0
      while (!dut.io.trapHalt.peek().litToBoolean && cyclesElapsed < 2000) {
        dut.clock.step(10)
        cyclesElapsed += 10
      }
      val cycles = dut.io.cycleCount.peek().litValue.toDouble
      val insts  = dut.io.instCount.peek().litValue.toDouble
      val ipc    = if (cycles > 0) insts / cycles else 0.0
      val sumVal = dut.io.registerFile(10).peek().litValue

      println(f"\n=======================================================")
      println(f"  DUAL-ISSUE C BENCHMARK 3: MATRIX MULTIPLICATION      ")
      println(f"=======================================================")
      println(f"  Matrix Total Sum        : $sumVal (Expected: 615)")
      println(f"  Total Cycles Elapsed    : ${cycles.toLong}")
      println(f"  Instructions Retired    : ${insts.toLong}")
      println(f"  Core IPC Throughput     : $ipc%.3f Instructions / Cycle")
      println(f"=======================================================\n")

      dut.io.registerFile(10).expect(615.U)
    }
  }
}

package lightningrv

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/**
  * CoreTest: Comprehensive Unit Verification Suite for LightningRV Dual-Issue DAE Core
  */
class CoreTest extends AnyFlatSpec with ChiselScalatestTester {

  "LightningRV Core" should "execute basic arithmetic 2 + 2 = 4 in register x10" in {
    val prog = Seq(
      BigInt("00200513", 16), // addi x10, x0, 2
      BigInt("00250513", 16), // addi x10, x10, 2
      BigInt("00000073", 16)  // ecall
    )
    test(new LightningRV(initWords = prog)) { dut =>
      dut.clock.setTimeout(0)
      while (!dut.io.trapHalt.peek().litToBoolean) {
        dut.clock.step(1)
      }
      dut.io.registerFile(10).expect(4.U)
    }
  }

  it should "execute shift and bitwise logical operations correctly" in {
    val prog = Seq(
      BigInt("00500513", 16), // addi x10, x0, 5
      BigInt("00251513", 16), // slli x10, x10, 2 (5 << 2 = 20)
      BigInt("00f57513", 16), // andi x10, x10, 15 (20 & 15 = 4)
      BigInt("00000073", 16)  // ecall
    )
    test(new LightningRV(initWords = prog)) { dut =>
      dut.clock.setTimeout(0)
      while (!dut.io.trapHalt.peek().litToBoolean) {
        dut.clock.step(1)
      }
      dut.io.registerFile(10).expect(4.U)
    }
  }

  it should "execute branch control flow loops using BNE" in {
    val prog = Seq(
      BigInt("00500513", 16), // addi x10, x0, 5   (counter = 5)
      BigInt("00000593", 16), // addi x11, x0, 0   (sum = 0)
      BigInt("00a585b3", 16), // add  x11, x11, x10 (sum += counter)
      BigInt("fff50513", 16), // addi x10, x10, -1 (counter--)
      BigInt("fe051ce3", 16), // bne  x10, x0, -8  (loop if counter != 0)
      BigInt("00000073", 16)  // ecall
    )
    test(new LightningRV(initWords = prog)) { dut =>
      dut.clock.setTimeout(0)
      while (!dut.io.trapHalt.peek().litToBoolean) {
        dut.clock.step(1)
      }
      dut.io.registerFile(11).expect(15.U)
    }
  }

  it should "execute sub-word memory access and emit MMIO console output" in {
    val prog = Seq(
      BigInt("04100093", 16), // addi x1, x0, 65 ('A')
      BigInt("80000137", 16), // lui  x2, 0x80000
      BigInt("00110023", 16), // sb   x1, 0(x2)
      BigInt("00000073", 16)  // ecall
    )
    test(new LightningRV(initWords = prog)) { dut =>
      dut.clock.setTimeout(0)
      while (!dut.io.trapHalt.peek().litToBoolean) {
        dut.clock.step(1)
      }
      dut.io.mmioCharValid.expect(true.B)
      dut.io.mmioChar.expect(65.U)
    }
  }

  it should "load binary files using BinLoader and execute C/Assembly machine code" in {
    val binWords = BinLoader.loadBinFile("benchmarks/fib.bin")
    test(new LightningRV(initWords = binWords)) { dut =>
      dut.clock.setTimeout(0)
      while (!dut.io.trapHalt.peek().litToBoolean) {
        dut.clock.step(1)
      }
      dut.io.registerFile(10).expect(55.U)
    }
  }

  it should "execute RV32M multi-cycle multiplication and division with Scoreboard RAW hazard handling" in {
    val prog = Seq(
      BigInt("00600513", 16), // addi x10, x0, 6
      BigInt("00700593", 16), // addi x11, x0, 7
      BigInt("02b50533", 16), // mul  x10, x10, x11 (6 * 7 = 42)
      BigInt("00200593", 16), // addi x11, x0, 2
      BigInt("02b54533", 16), // div  x10, x10, x11 (42 / 2 = 21)
      BigInt("00000073", 16)  // ecall
    )
    test(new LightningRV(initWords = prog)) { dut =>
      dut.clock.setTimeout(0)
      while (!dut.io.trapHalt.peek().litToBoolean) {
        dut.clock.step(1)
      }
      dut.io.registerFile(10).expect(21.U)
    }
  }
}

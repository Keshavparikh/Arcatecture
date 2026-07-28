package lightningrv

import java.nio.ByteBuffer
import java.nio.file.{Files, Paths}
import scala.collection.mutable.ArrayBuffer

/**
  * BinLoader: Utility helper for loading compiled C / Assembly binaries into LightningRV Scratchpad memory.
  */
object BinLoader {
  /**
    * Load a raw binary (.bin) file produced by riscv64-unknown-elf-gcc and objcopy -O binary
    * Converts byte array into 32-bit little-endian words for Scratchpad initialization.
    */
  def loadBinFile(filePath: String): Seq[BigInt] = {
    val path = Paths.get(filePath)
    if (!Files.exists(path)) {
      throw new IllegalArgumentException(s"Binary file not found: $filePath")
    }
    val bytes = Files.readAllBytes(path)
    val buffer = ByteBuffer.wrap(bytes).order(java.nio.ByteOrder.LITTLE_ENDIAN)
    
    val words = ArrayBuffer[BigInt]()
    while (buffer.remaining() >= 4) {
      val word = Integer.toUnsignedLong(buffer.getInt())
      words += BigInt(word)
    }
    words.toSeq
  }

  /**
    * Load a hex text file (.hex) where each line contains an 8-character hex instruction string (e.g. 00200093)
    */
  def loadHexFile(filePath: String): Seq[BigInt] = {
    val source = scala.io.Source.fromFile(filePath)
    try {
      source.getLines()
        .map(_.trim)
        .filter(line => line.nonEmpty && !line.startsWith("//") && !line.startsWith("#"))
        .map(hex => BigInt(hex, 16))
        .toSeq
    } finally {
      source.close()
    }
  }
}

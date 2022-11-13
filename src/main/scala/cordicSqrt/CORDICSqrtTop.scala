package cordicSqrt

import chisel3._
import chisel3.util._
import chisel3.experimental._

object SqrtDatatype extends ChiselEnum {
  val FLOAT, DOUBLE = Value
}

object FaultFlag extends ChiselEnum {
  val inexact     = Value(0.U)
  val underflow   = Value(1.U)
  val overflow    = Value(2.U)
  val div_by_zero = Value(3.U)
  val invalid     = Value(4.U)
}

case class CORDICSqrtOutput() extends Bundle {
  val data   = UInt(64.W)
  val fflags = UInt(5.W)
}

case class FloatFields() extends Bundle {
  val mantissa = UInt(53.W)
  val exponent = UInt(11.W)
  val sign     = UInt(1.W)
  val mant_rnd = UInt(1.W) // Stores mantissa(0) if it was shifted right
}

/**
  * Compute floating-point square root with CORDIC algorithm.
  * Dynamically selectable between float and double
  */
class CORDICSqrtTop extends Module {
  val io = IO(new Bundle {
    val in = Flipped(ValidIO(UInt(64.W)))
    val datatype = Input(SqrtDatatype())
    val out = ValidIO(CORDICSqrtOutput())
  })

  // Submodules
  val preprocessor = Module(new PreProcessor)

  // Pipeline registers
  val preproc_to_cordic = Reg(FloatFields())

  // Initial values
  io.out.bits.data            := 0.U
  io.out.bits.fflags          := 0.U
  io.out.valid                := false.B
  preprocessor.io.in.datatype := io.datatype
  preprocessor.io.in.data     := 0.U

  when (io.in.valid) {
    preprocessor.io.in.data := io.in.bits
    // If preprocessor found an easy solution
    when (preprocessor.io.out.data.valid) {
      io.out <> preprocessor.io.out.data
      io.out.valid     := true.B
    } .otherwise {
      preproc_to_cordic.mantissa := preprocessor.io.out.mant
      preproc_to_cordic.exponent := preprocessor.io.out.expo
      preproc_to_cordic.sign     := 0.U // sign cannot be negative...
      preproc_to_cordic.mant_rnd := preprocessor.io.out.mant_round
    }
  }
}

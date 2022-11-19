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

  val preprocessor = Module(new PreProcessor)
  val cordic_iter  = Module(new CORDICSqrt(width=100, iterations=100))

  // Registers
  val in_r    = RegNext(io.in)
  val out_r   = Reg(CORDICSqrtOutput())
  val valid_r = RegInit(false.B)

  // Assignments
  io.out.bits  <> out_r
  io.out.valid := valid_r

  // Initial values
  out_r.data                  := 0.U
  out_r.fflags                := 0.U
  preprocessor.io.in.datatype := io.datatype
  preprocessor.io.in.data     := 0.U

  when (in_r.valid) {
    preprocessor.io.in.data := in_r.bits
    // If preprocessor found an easy solution
    when (preprocessor.io.out.data.valid) {
      out_r <> preprocessor.io.out.data
      valid_r := true.B
    }
  }
}

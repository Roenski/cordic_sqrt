package cordicSqrt

import chisel3._
import chisel3.util._
import chisel3.experimental._

/**
  * Calculates squareroot exponent and readjusts mantissa
  * and handles special cases.
  *
  */
class PreProcessor extends Module {
  val io = IO(new Bundle {
    val in = Input(new Bundle {
      val data = UInt(64.W)
      val datatype = SqrtDatatype()
    })
    val out = Output(new Bundle {
      val mant = UInt(52.W)
      val mant_round = UInt(1.W)
      val expo = UInt(11.W)
      val data = ValidIO(CORDICSqrtOutput())
    })
  })

  val bias = 1023.U

  val exponent = io.in.data(62,52)
  val mant_with_hidden = Cat(
    Mux(exponent === bias, 0.U, 1.U), io.in.data(51,0)
  )
  val mantissa = io.in.data(51,0)
  val sign     = io.in.data(63)
  val mant_rnd = WireDefault(0.U(1.W))
  val new_expo = WireDefault(0.U(11.W))
  val new_mant = WireDefault(0.U(52.W))

  io.out.mant       := new_mant
  io.out.expo       := new_expo
  io.out.mant_round := mant_rnd

  // Calculate new exponent
  // and adjust mantissa when necessary
  when (exponent === bias) {
    new_expo := exponent
  } .elsewhen (exponent(0)) {
    // Exponent is odd (or even, after subtracting bias)
    new_expo := (exponent >> 1) + (bias >> 1) + 1.U
  } .otherwise {
    // Exponent is even (or odd, after subtracting bias)
    // Both exponent and bias have an LSB, thus we can forget them
    // and divide by 2 by shifting right, and then just add 1
    new_expo := ((exponent + 1.U) >> 1) + (bias >> 1) + 1.U
    new_mant := mant_with_hidden >> 1 
    mant_rnd := mant_with_hidden(0)
  }


  // Special cases
  // If one of them matches, valid is set true
  val valid          = WireDefault(false.B)
  val sign_valid     = WireDefault(0.U(1.W))
  val mantissa_valid = WireDefault(0.U(52.W))
  val exponent_valid = WireDefault(0.U(11.W))
  val fflags_valid   = VecInit.fill(5) {false.B}
  val data_valid     = Mux(
    valid, Cat(sign_valid, exponent_valid, mantissa_valid), 0.U(64.W)
  )

  io.out.data.bits.data   := data_valid
  io.out.data.bits.fflags := fflags_valid.asUInt
  io.out.data.valid       := valid

  val is_zero = (mantissa === 0.U && exponent === 0.U)
  val is_nan  = exponent.andR && mantissa.orR
  val is_qnan = (is_nan && mantissa(51)) 
  val is_snan = (is_nan && !mantissa(51))
  val is_inf  = exponent.andR && !mantissa.orR

  when (is_zero) {
    // no flags
    sign_valid     := sign
    mantissa_valid := mantissa
    exponent_valid := exponent
    valid          := true.B
  } .elsewhen (is_qnan) {
    // no flags
    sign_valid     := sign
    mantissa_valid := mantissa
    exponent_valid := exponent
    valid          := true.B
  } .elsewhen (is_snan) {
    // invalid flag
    sign_valid     := sign
    mantissa_valid := mantissa
    exponent_valid := exponent
    valid          := true.B
    fflags_valid(FaultFlag.invalid.asUInt) := true.B
  } .elsewhen (sign) {
    // output a qNan
    // and an invalid flag
    sign_valid     := sign
    mantissa_valid := Cat(1.U, mantissa(50,0))
    exponent_valid := Fill(11, 1.U)
    valid          := true.B
    fflags_valid(FaultFlag.invalid.asUInt) := true.B
  } .elsewhen (is_inf) {
    // no flags
    sign_valid     := sign
    mantissa_valid := mantissa
    exponent_valid := exponent
    valid          := true.B
  }

}
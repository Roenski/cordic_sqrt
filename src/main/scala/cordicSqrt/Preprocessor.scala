package cordicSqrt

import chisel3._
import chisel3.util._
import chisel3.experimental._

/** Calculates squareroot exponent, adjusts mantissa and handles special cases.
  */
class PreProcessor extends Module {

  val io = IO(new Bundle {

    val in = Input(new Bundle {
      val data     = UInt(64.W)
      val datatype = SqrtDatatype()
    })

    val out = Output(new Bundle {
      val mantissa = UInt(54.W)
      val exponent = UInt(11.W)
      val data     = ValidIO(CORDICSqrtOutput())
    })

  })

  val bias = 1023.U

  val mantissa    = io.in.data(51, 0)
  val exponent    = io.in.data(62, 52)
  val sign        = io.in.data(63)
  val hiddenBit   = (exponent =/= 0.U)
  val newExponent = WireDefault(0.U(11.W))
  // New mantissa will include:
  // hidden bit - 1 bit
  // old mantissa - 52 bits
  // round bit caused by bit shift - 1 bit
  val newMantissa = WireDefault(0.U(54.W))

  io.out.mantissa := newMantissa
  io.out.exponent := newExponent

  // Calculate new exponent
  // and adjust mantissa when necessary
  when(exponent === bias) {
    newExponent := exponent
    newMantissa := Cat(hiddenBit, mantissa)
  }.elsewhen(exponent(0)) {
    // Exponent is odd (or even, after subtracting bias)
    // Exponent divided by 2, remainder 0
    newExponent := (exponent >> 1) + (bias >> 1) + 1.U
    newMantissa := Cat(hiddenBit, mantissa)
  }.otherwise {
    // Exponent is even (or odd, after subtracting bias)
    // Both exponent and bias have an LSB, thus we can forget them
    // and divide by 2 by shifting right, and then just add 1
    // Exponent divided by 2, remainder 1
    // Mantissa is divided by 2, exponent rounded upwards
    newExponent := ((exponent + 1.U) >> 1) + (bias >> 1) + 1.U
    newMantissa := Cat(0.U, hiddenBit, mantissa)
  }

  // Special cases
  // If one of them matches, valid is set true
  val valid         = WireDefault(false.B)
  val signValid     = WireDefault(0.U(1.W))
  val mantissaValid = WireDefault(0.U(52.W))
  val exponentValid = WireDefault(0.U(11.W))
  val fflagsValid   = VecInit.fill(5) { false.B }

  val data_valid = Mux(
    valid,
    Cat(signValid, exponentValid, mantissaValid),
    0.U(64.W)
  )

  io.out.data.bits.data   := data_valid
  io.out.data.bits.fflags := fflagsValid.asUInt
  io.out.data.valid       := valid

  val isZero = (mantissa === 0.U && exponent === 0.U)
  val isNan  = exponent.andR && mantissa.orR
  val isQnan = (isNan && mantissa(51))
  val isSnan = (isNan && !mantissa(51))
  val isInf  = exponent.andR && !mantissa.orR

  when(isZero) {
    // no flags
    signValid     := sign
    mantissaValid := mantissa
    exponentValid := exponent
    valid         := true.B
  }.elsewhen(isQnan) {
    // no flags
    signValid     := sign
    mantissaValid := mantissa
    exponentValid := exponent
    valid         := true.B
  }.elsewhen(isSnan) {
    // invalid flag
    signValid                             := sign
    mantissaValid                         := mantissa
    exponentValid                         := exponent
    valid                                 := true.B
    fflagsValid(FaultFlag.invalid.asUInt) := true.B
  }.elsewhen(sign) {
    // output a qNan
    // and an invalid flag
    signValid                             := sign
    mantissaValid                         := Cat(1.U, mantissa(50, 0))
    exponentValid                         := Fill(11, 1.U)
    valid                                 := true.B
    fflagsValid(FaultFlag.invalid.asUInt) := true.B
  }.elsewhen(isInf) {
    // no flags
    signValid     := sign
    mantissaValid := mantissa
    exponentValid := exponent
    valid         := true.B
  }

}

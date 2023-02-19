package cordicSqrt

import chisel3._
import chisel3.util._
import chisel3.experimental._
import SqrtDatatype._
import FloatConsts._

/** Calculates squareroot exponent, adjusts mantissa and handles special cases.
  */
class PreProcessor(val datatype: SqrtDatatype) extends Module {

  val consts = FloatConsts(datatype)

  val io = IO(new Bundle {

    val in = Input(new Bundle {
      val data = UInt(consts.dataLength.W)
      // val datatype = SqrtDatatype()
    })

    val out = Output(new Bundle {
      val mantissa = UInt((consts.mantissaLength+2).W)
      val exponent = UInt((consts.exponentLength).W)
      val data     = ValidIO(CORDICSqrtOutput(datatype))
    })

  })


  val mantissa = io.in.data(consts.mantissaLength - 1, 0)

  val exponent = io.in.data(
    consts.mantissaLength + consts.exponentLength - 1,
    consts.mantissaLength
  )

  val sign        = io.in.data(consts.dataLength - 1)
  val hiddenBit   = (exponent =/= 0.U)
  val newExponent = WireDefault(0.U(consts.exponentLength.W))
  // New mantissa will include:
  // hidden bit - 1 bit
  // old mantissa - 52 bits
  // round bit caused by bit shift - 1 bit
  val newMantissa = WireDefault(0.U((consts.mantissaLength + 2).W))

  io.out.mantissa := newMantissa
  io.out.exponent := newExponent

  // Calculate new exponent
  // and adjust mantissa when necessary
  when(exponent === consts.bias.U) {
    newExponent := exponent
    newMantissa := Cat(hiddenBit, mantissa, 0.U)
  }.elsewhen(exponent(0)) {
    // Exponent is odd (or even, after subtracting bias)
    // Exponent divided by 2, remainder 0
    newExponent := (exponent >> 1) + (consts.bias.U >> 1) + 1.U
    newMantissa := Cat(hiddenBit, mantissa, 0.U)
  }.otherwise {
    // Exponent is even (or odd, after subtracting bias)
    // Both exponent and bias have an LSB, thus we can forget them
    // and divide by 2 by shifting right, and then just add 1
    // Exponent divided by 2, remainder 1
    // Mantissa is divided by 2, exponent rounded upwards
    newExponent := ((exponent + 1.U) >> 1) + (consts.bias.U >> 1) + 1.U
    newMantissa := Cat(0.U, hiddenBit, mantissa)
  }

  // Special cases
  // If one of them matches, special is set true
  val special       = WireDefault(false.B)
  val signValid     = WireDefault(0.U(1.W))
  val mantissaValid = WireDefault(0.U(consts.mantissaLength.W))
  val exponentValid = WireDefault(0.U(consts.exponentLength.W))
  val fflagsValid   = VecInit.fill(5) { false.B }

  io.out.data.bits.mantissa  := mantissaValid
  io.out.data.bits.exponent  := exponentValid
  io.out.data.bits.signBit   := signValid
  io.out.data.bits.guardBit  := 0.U
  io.out.data.bits.roundBit  := 0.U
  io.out.data.bits.stickyBit := 0.U
  io.out.data.bits.fflags    := fflagsValid.asUInt
  io.out.data.valid          := true.B
  io.out.data.bits.special   := special

  val isZero = (mantissa === 0.U && exponent === 0.U)
  val isNan  = exponent.andR && mantissa.orR
  val isQnan = (isNan && mantissa(consts.mantissaLength - 1))
  val isSnan = (isNan && !mantissa(consts.mantissaLength - 1))
  val isInf  = exponent.andR && !mantissa.orR

  when(isZero) {
    // no flags
    signValid     := sign
    mantissaValid := mantissa
    exponentValid := exponent
    special       := true.B
  }.elsewhen(isQnan) {
    // no flags
    signValid     := sign
    mantissaValid := mantissa
    exponentValid := exponent
    special       := true.B
  }.elsewhen(isSnan) {
    // invalid flag
    signValid                             := sign
    mantissaValid                         := mantissa
    exponentValid                         := exponent
    special                               := true.B
    fflagsValid(FaultFlag.invalid.asUInt) := true.B
  }.elsewhen(sign) {
    // output a qNan
    // and an invalid flag
    signValid                             := sign
    mantissaValid                         := Cat(1.U, mantissa(consts.mantissaLength - 2, 0))
    exponentValid                         := Fill(consts.exponentLength, 1.U)
    special                               := true.B
    fflagsValid(FaultFlag.invalid.asUInt) := true.B
  }.elsewhen(isInf) {
    // no flags
    signValid     := sign
    mantissaValid := mantissa
    exponentValid := exponent
    special       := true.B
  }

}

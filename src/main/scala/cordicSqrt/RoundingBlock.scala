package cordicSqrt

import chisel3._
import chisel3.util._
import chisel3.experimental._
import SqrtDatatype._
import FloatConsts._

case class RoundingBlockOutput(datatype: SqrtDatatype) extends Bundle
    with datatypeMux {
  val data   = UInt(datatypeMux(datatype, 32, 64).W)
  val fflags = UInt(5.W)
}

class RoundingBlock(val datatype: SqrtDatatype) extends Module {

  val io = IO(new Bundle {
    val in  = Input(Flipped(ValidIO(CORDICSqrtOutput(datatype))))
    val out = Output(ValidIO(RoundingBlockOutput(datatype)))
  })

  // Wires
  val shiftedMantissa = WireDefault(0.U)
  val roundedMantissa = WireDefault(0.U)
  val finalExponent   = WireDefault(0.U)
  val usedGuardBit    = WireDefault(false.B)
  val round           = WireDefault(false.B)
  val tie             = WireDefault(false.B)
  val overflowDanger  = WireDefault(false.B)
  val overflow        = WireDefault(false.B)

  when (!io.in.bits.special.asBool) {
    when(io.in.bits.mantissa.head(1) === 0.U) {
      shiftedMantissa := Cat(io.in.bits.mantissa.tail(1), io.in.bits.guardBit)
      usedGuardBit    := true.B
    }.otherwise {
      shiftedMantissa := io.in.bits.mantissa
      usedGuardBit    := false.B
    }

    overflowDanger := shiftedMantissa.andR

    when(usedGuardBit) {
      // TODO: different rounding methods
      round := io.in.bits.roundBit.asBool && io.in.bits.stickyBit.asBool
      tie   := io.in.bits.roundBit.asBool && !io.in.bits.stickyBit.asBool
    }.otherwise {
      round := io.in.bits.guardBit.asBool &&
      (io.in.bits.roundBit.asBool || io.in.bits.stickyBit.asBool)
      tie := io.in.bits.guardBit.asBool &&
      (!io.in.bits.roundBit.asBool && !io.in.bits.stickyBit.asBool)
    }

    when (round) {
      roundedMantissa := shiftedMantissa + 1.U
      overflow := overflowDanger
    } .otherwise {
      roundedMantissa := shiftedMantissa
      overflow := false.B
    }

    when (overflow && !usedGuardBit) {
      finalExponent := io.in.bits.exponent + 1.U
    } .otherwise {
      finalExponent := io.in.bits.exponent
    }

  }.otherwise {
    finalExponent   := io.in.bits.exponent
    roundedMantissa := io.in.bits.mantissa
  }

  io.out.bits.data := Cat(
    io.in.bits.signBit,
    finalExponent,
    roundedMantissa.tail(1)
  )

  io.out.bits.fflags := io.in.bits.fflags
  io.out.valid       := io.in.valid

}

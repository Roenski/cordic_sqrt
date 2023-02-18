package cordicSqrt

import chisel3._
import chisel3.util._
import chisel3.experimental._
import SqrtDatatype._
import FloatConsts._

case class RoundingBlockOutput(datatype: SqrtDatatype) extends Bundle with datatypeMux {
  val data   = UInt(datatypeMux(datatype, 32, 64).W)
  val fflags = UInt(5.W)
}

class RoundingBlock(val datatype: SqrtDatatype) extends Module {

  val io = IO(new Bundle {
    val in = Input(Flipped(ValidIO(CORDICSqrtOutput(datatype))))
    val out = Output(ValidIO(RoundingBlockOutput(datatype)))
  })

  io.out.bits.data := Cat(io.in.bits.signBit, io.in.bits.exponent, io.in.bits.mantissa.tail(1))
  io.out.bits.fflags := io.in.bits.fflags
  io.out.valid := io.in.valid

}
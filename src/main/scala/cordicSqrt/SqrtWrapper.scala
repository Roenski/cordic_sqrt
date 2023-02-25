package cordicSqrt

import chisel3._
import chisel3.util._
import chisel3.experimental._

import SqrtDatatype._

case class SqrtWrapperIO(datatype: SqrtDatatype, bitWidth: Int) extends Bundle {
  val in = Flipped(ValidIO(UInt(bitWidth.W)))
  val out = Output(ValidIO(RoundingBlockOutput(datatype)))
}

class SqrtWrapper(val datatype: SqrtDatatype) extends Module with datatypeMux {

  val bitWidth = {
    if (datatype == SqrtDatatype.SINGLE) 32
    else 64
  }

  val io = IO(SqrtWrapperIO(datatype, bitWidth))

  // Submodules
  val sqrtTop       = Module(new CORDICSqrtTop(datatype))
  val roundingBlock = Module(new RoundingBlock(datatype))

  // Connections
  sqrtTop.io.in       <> io.in
  roundingBlock.io.in <> sqrtTop.io.out
  io.out              <> roundingBlock.io.out
}
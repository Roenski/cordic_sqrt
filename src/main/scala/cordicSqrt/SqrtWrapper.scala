package cordicSqrt

import chisel3._
import chisel3.util._
import chisel3.experimental._

import SqrtDatatype._

class SqrtWrapper(val datatype: SqrtDatatype) extends Module with datatypeMux {

  val bitWidth = {
    if (datatype == SqrtDatatype.FLOAT) 32
    else 64
  }

  val io = IO(new Bundle {
    val in = Flipped(ValidIO(UInt(bitWidth.W)))
    val out = Output(ValidIO(RoundingBlockOutput(datatype)))
  })

  // Submodules
  val sqrtTop       = Module(new CORDICSqrtTop(datatype))
  val roundingBlock = Module(new RoundingBlock(datatype))

  // Connections
  sqrtTop.io.in       <> io.in
  roundingBlock.io.in <> sqrtTop.io.out
  io.out              <> roundingBlock.io.out
}
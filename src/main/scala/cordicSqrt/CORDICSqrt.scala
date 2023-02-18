package cordicSqrt

import chisel3._
import chisel3.util._
import chisel3.experimental._

/** One iteration of CORDIC for sqrt
  */
class CORDICSqrt(width: Int, iterations: Int) extends Module {

  val in = IO(Input(new Bundle {
    val xn   = SInt(width.W)
    val yn   = SInt(width.W)
    val iter = UInt(log2Up(iterations).W)
  }))

  val out = IO(Output(new Bundle {
    val xn1 = SInt(width.W)
    val yn1 = SInt(width.W)
  }))

  val xtemp = in.xn >> in.iter
  val ytemp = in.yn >> in.iter

  when(in.yn < 0.S) {
    out.xn1 := in.xn + ytemp
    out.yn1 := in.yn + xtemp
  }.otherwise {
    out.xn1 := in.xn - ytemp
    out.yn1 := in.yn - xtemp
  }

}

package cordicSqrt

import chisel3._
import chisel3.util._
import chisel3.experimental._

/**
  * One iteration of CORDIC for sqrt
  *
  */
class CORDICSqrt(width: Int, iterations: Int) extends Module {
  val in = IO(Input(new Bundle {
    val xn   = UInt(width.W)
    val yn   = UInt(width.W)
    val iter = UInt((math.log(iterations)/math.log(2)).toInt.W)
  }))
  val out = IO(Output(new Bundle {
    val xn1 = UInt(width.W)
    val yn1 = UInt(width.W)
  }))

  val xtemp = in.xn >> in.iter
  val ytemp = in.yn >> in.iter

  when (in.yn(width-1) === 1.U) {
    out.xn1 := in.xn + ytemp 
    out.yn1 := in.yn + xtemp
  } .otherwise {
    out.xn1 := in.xn - ytemp 
    out.yn1 := in.yn - xtemp
  }
}
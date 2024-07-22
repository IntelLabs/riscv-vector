//package grapecoveDcache
//
//import chisel3._
//import chisel3.util._
//import freechips.rocketchip.tilelink._
//import _root_.circt.stage.ChiselStage
//
//object MSHRReqType {
//  val width = 2
//
//  def norm   = 0.U(width.W)
//  def AMO    = 1.U(width.W)
//  def MMIO   = 2.U(width.W)
//  def Bypass = 3.U(width.W)
//}
//
//class IOMSHR(id: Int)(
//    implicit edge: TLEdgeOut
//) extends Module {
//  val io = IO(new Bundle {
//    val pipeReq = new Bundle() {
//      val reqType = Input(UInt(MSHRReqType.width.W))
//      val reqCMD = Input
//    }
//  })
//}

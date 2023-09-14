// package darecreek

// import chisel3._
// import chisel3.util._

// object DummyMemParam {
//   val addrWidth = 8
//   val memDepth = 1 << addrWidth
// }
// import DummyMemParam._

// class DummyMemIO extends Bundle {
//   val raddr = Input(UInt((addrWidth).W))
//   val rdata = Output(UInt(64.W))
//   val wen = Input(Bool())
//   val waddr = Input(UInt((addrWidth).W))
//   val wdata = Input(UInt(64.W))
// }

// // temp: only unit-stride
// class TempLSU extends Module {
//   val io = IO(new Bundle {
//     val fromIQ = new Bundle {
//       val ld = Flipped(Decoupled(new VLdInput))
//       val st = Flipped(Decoupled(new VStInput))
//     }
//     val ld = ValidIO(new VLdOutput)
//     val st = ValidIO(new VStOutput)
//     val mem = Vec(NLanes, Flipped(new DummyMemIO))
//   })

//   // Load
//   val ldBaseAddr = io.fromIQ.ld.bits.rs1
//   val ldExpdBaseAddr = ldBaseAddr + io.fromIQ.ld.bits.uop.expdIdx * NLanes.U
//   val ldAddrLane = Wire(Vec(NLanes, UInt(xLen.W)))
//   for (i <- 0 until NLanes) {
//     ldAddrLane(i) := ldExpdBaseAddr + i.U
//     io.mem(i).raddr := ldAddrLane(i)
//   }
//   io.fromIQ.ld.ready := true.B
//   io.ld.valid := RegNext(io.fromIQ.ld.fire)
//   io.ld.bits.uop := RegNext(io.fromIQ.ld.bits.uop)
//   io.ld.bits.vd := RegNext(VecInit(io.mem.map(_.rdata)))

//   // Store
//   val stBaseAddr = io.fromIQ.st.bits.rs1
//   val stExpdBaseAddr = stBaseAddr + io.fromIQ.st.bits.uop.expdIdx * NLanes.U
//   val stAddrLane = Wire(Vec(NLanes, UInt(xLen.W)))
//   for (i <- 0 until NLanes) {
//     stAddrLane(i) := stExpdBaseAddr + i.U
//     io.mem(i).waddr := stAddrLane(i)
//     io.mem(i).wen := io.fromIQ.st.fire
//     io.mem(i).wdata := io.fromIQ.st.bits.vs3(i)
//   }
//   io.fromIQ.st.ready := true.B
//   io.st.valid := RegNext(io.fromIQ.st.fire)
//   io.st.bits.uop := RegNext(io.fromIQ.st.bits.uop)

// }

// // temp: only 64-bit element
// class DummyMem extends Module {
//   val io = IO(new Bundle {
//     val mem = Vec(NLanes, new DummyMemIO)
//     val preLoadMem = Flipped(ValidIO(UInt(2.W))) // vsew    0: 8,  1: 16,  2: 32,  3: 64
//   })
//   val ram = RegInit(VecInit(Seq.tabulate(memDepth)(x => x.U(64.W))))
//   for (i <- 0 until NLanes) {
//     io.mem(i).rdata := ram(io.mem(i).raddr)
//   }

//   when(io.preLoadMem.valid) {  //preloadMem values: 0, 1, 2, 3, 4, ...
//     for (k <- 0 until memDepth) {
//       ram(k) := Mux1H(Seq(
//           (io.preLoadMem.bits === 3.U) -> k.U,
//           (io.preLoadMem.bits === 2.U) -> Cat(((2*k+1)%256).U(8.W), ((2*k)%256).U(8.W)),
//           (io.preLoadMem.bits === 1.U) -> Cat(((4*k+3)%256).U(8.W), ((4*k+2)%256).U(8.W), ((4*k+1)%256).U(8.W), ((4*k)%256).U(8.W)),
//           (io.preLoadMem.bits === 0.U) -> Cat(((8*k+7)%256).U(8.W), ((8*k+6)%256).U(8.W), ((8*k+5)%256).U(8.W), 
//                     ((8*k+4)%256).U(8.W), ((8*k+3)%256).U(8.W), ((8*k+2)%256).U(8.W), ((8*k+1)%256).U(8.W), ((8*k)%256).U(8.W))
//       ))
//     }
//   }.otherwise {
//     for (i <- 0 until NLanes) {
//       when (io.mem(i).wen) {
//         ram(io.mem(i).waddr) := io.mem(i).wdata
//       }
//     }
//   }
// }
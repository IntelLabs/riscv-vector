package smartVector

import chisel3._
import chisel3.util._

import SmartParam._
import freechips.rocketchip.rocket.HellaCacheExceptions


// RVU (Rocket Vector Interface)

class VInfo extends Bundle {
    val vl     = (UInt(bVL.W))
    val vstart = (UInt(bVstart.W))
    val vma    = (Bool())
    val vta    = (Bool())
    val vsew   = (UInt(3.W))
    val vlmul  = (UInt(3.W))
    val vxrm   = (UInt(2.W))
    val frm    = (UInt(3.W))
}

class RVUissue extends Bundle {
    val inst   = UInt(32.W)
    val rs1    = UInt(64.W)
    val rs2    = UInt(64.W)
    val vInfo  = new VInfo
}

class RVUMemoryReq extends Bundle {
    val addr = UInt(64.W)
    val size = UInt(log2Ceil(64 + 1).W)
    val signed = Bool()
    val cmd = UInt(3.W)
    val phys = Bool() 
    val idx = UInt(8.W) // ldst queue index

    // store data
    val data = UInt(64.W)
    val mask = UInt(8.W)
}

class RVUMemoryResp extends Bundle {
    val data = UInt(64.W)
    val mask = UInt(8.W)
    val replay = Bool()
    val has_data = Bool()

}

class RVUMemory extends Bundle {
    val req  = Decoupled(new RVUMemoryReq)
    val resp = Flipped(Valid(new RVUMemoryResp))
    val xcpt = Input(Bool())
    val busy = Input(Bool())
}

class RVUCommit extends Bundle {
    val commit_vld      = Output(Bool())
    val return_data_vld = Output(Bool()) // need to update scalar rf
    val return_data     = Output(UInt(64.W))
    val return_reg_idx  = Output(UInt(5.W))
    val exception_vld   = Output(Bool())
    val illegal_inst    = Output(Bool())
    val update_vl       = Output(Bool())
    val update_vl_data  = Output(UInt(5.W))
}

class RVUExtra extends Bundle {
    //val vpu_ready = Output(Bool())
} 



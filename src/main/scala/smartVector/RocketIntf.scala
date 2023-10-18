package smartVector

import chisel3._
import chisel3.util._

import SmartParam._


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

class RVUMemory extends Bundle {
    val lsu_req_valid      = Output(Bool())
    val lsu_req_ld         = Output(Bool())
    val lsu_req_addrs      = Output(UInt(64.W))
    val lsu_req_data_width = Output(UInt(3.W))
    val st_req_data        = Output(UInt(64.W))
    val ld_resp_data       = Input(UInt(64.W))
    val lsu_resp_valid     = Input(Bool())
    val lsu_resp_excp      = Input(Bool())
}

class RVUCommit extends Bundle {
    val commit_vld      = Output(Bool())
    val return_data_vld = Output(Bool())
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



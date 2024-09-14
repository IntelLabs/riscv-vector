package smartVector

import chisel3._
import chisel3.util._

class VCommit extends Module {

    val io = IO(new Bundle{
        val in = new Bundle{
            val commitInfo = Input(ValidIO(new CommitInfo))
            val excpInfo   = Input(new ExcpInfo)
            val vLSUXcpt = Input (new VLSUXcpt)
        }
        val out = new Bundle{
            val commitInfo = Output(new RVUCommit)
        }        
    }
    )

    io.out.commitInfo.commit_vld             := io.in.commitInfo.valid || io.in.excpInfo.exception_vld || io.in.vLSUXcpt.exception_vld || io.in.vLSUXcpt.update_vl
    io.out.commitInfo.exception_vld          := io.in.excpInfo.exception_vld || io.in.vLSUXcpt.exception_vld
    io.out.commitInfo.illegal_inst           := io.in.excpInfo.illegalInst
    io.out.commitInfo.update_vl              := io.in.vLSUXcpt.update_vl
    io.out.commitInfo.update_vl_data         := io.in.vLSUXcpt.update_data
    io.out.commitInfo.return_data_vld        := io.in.commitInfo.bits.scalarRegWriteEn
    io.out.commitInfo.return_data_float_vld  := io.in.commitInfo.bits.floatRegWriteEn || (io.in.excpInfo.update_float && io.in.excpInfo.exception_vld)
    io.out.commitInfo.return_reg_idx         := Mux(io.in.excpInfo.exception_vld, io.in.excpInfo.reg_idx, io.in.commitInfo.bits.ldest)
    io.out.commitInfo.return_data            := io.in.commitInfo.bits.data
    io.out.commitInfo.vxsat                  := io.in.commitInfo.bits.vxsat
    io.out.commitInfo.fflags                 := io.in.commitInfo.bits.fflags
    io.out.commitInfo.xcpt_cause             <> io.in.vLSUXcpt.xcpt_cause
    io.out.commitInfo.xcpt_addr              := io.in.vLSUXcpt.xcpt_addr

}

package smartVector

import chisel3._
import chisel3.util._

class VCommit extends Module {

    val io = IO(new Bundle{
        val in = new Bundle{
            val commitInfo = Input(ValidIO(new CommitInfo))
            val excpInfo   = Input(new ExcpInfo)
        }
        val out = new Bundle{
            val commitInfo = ValidIO(new RVUCommit)
        }        
    }
    )

    io.out.commitInfo.valid := io.in.commitInfo.valid || io.in.excpInfo.exception_vld
    io.out.commitInfo.bits.illegal_inst    := io.in.excpInfo.illegalInst
    io.out.commitInfo.bits.update_vl       := io.in.excpInfo.update_vl
    io.out.commitInfo.bits.update_vl_data  := io.in.excpInfo.update_data
    io.out.commitInfo.bits.return_data_vld := io.in.commitInfo.bits.scalarRegWriteEn
    io.out.commitInfo.bits.return_reg_idx  := io.in.commitInfo.bits.ldest
    io.out.commitInfo.bits.return_data     := io.in.commitInfo.bits.data
    

}

package smartVector

import chisel3._
import chisel3.util._

class VCommit extends Module {

    val io = IO(new Bundle{
        val in = new Bundle{
            val commitInfo = Input(ValidIO(new CommitInfo))
            
        }
        val out = new Bundle{
            val commitInfo = ValidIO(new CommitInfo)
        }        
    }
    )

    io.out := io.in

}

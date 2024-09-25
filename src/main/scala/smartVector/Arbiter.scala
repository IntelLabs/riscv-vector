//package smartVector
//
//
//class AgeArbiter[T <: Data](val gen: T, val n: Int) extends Module {
//  val io = IO(new ArbiterIO(gen, n))
//  
//  // 假设每个输入包含一个age字段 (UInt类型)
//  val ages = io.in.map(_.age)  // 提取每个输入的年龄
//  
//  // 初始化 chosen 为第一个输入，或者根据你需要的逻辑选择一个默认值
//  io.chosen := 0.U
//  io.out.bits := io.in(0).bits
//  
//  def selectOlder(bits1: UInt, bits2: UInt): UInt = {
//    Mux(bits1(4) === bits2(4), 
//      Mux(bits1(3,0) > bits2(3,0), bits1, bits2),  // 当 bits1(4) == bits2(4) 时，选取
//      Mux(bits1(3,0) < bits2(3,0), bits1, bits2)   // 当 bits1(4) != bits2(4) 时，比较 bits1(4) 和 bits2(4)
//    )
//  }
//
//  // 找到年龄最大的输入
//  var maxAge = ages(0)
//  for (i <- 1 until n) {
//    maxAge = selectOlder(maxAge, ages(i))
//  }
//
//  // 生成授权信号：这部分假设你还是基于valid来进行控制
//  val grant = ArbiterCtrl(io.in.map(_.valid))
//  for ((in, g) <- io.in.zip(grant))
//    in.ready := g && io.out.ready
//
//  // 输出 valid 信号
//  io.out.valid := io.in(io.chosen).valid
//}


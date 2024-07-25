import chisel3._
import circt.stage.ChiselStage
import chisel3.stage.ChiselOption

object CPU_Main extends App {
    var firtool_options = Array("-disable-all-randomization", 
                                "-strip-debug-info",
                                "-strip-fir-debug-info",
                                "-O=release",
                                "--ignore-read-enable-mem",
                                "--lower-memories",
                                "--lowering-options=disallowLocalVariables, explicitBitcast, disallowMuxInlining, disallowExpressionInliningInPorts, verifLabels",
                                "-o=verilog/",
                                "-split-verilog",
                                //"--scalarize-top-module"
                                
                                )
    if(System.getProperties().getProperty("mode") != "sim"){
        firtool_options = firtool_options ++ Array("--vb-to-bv")
    }
    
    ChiselStage.emitSystemVerilogFile(
        new FreeList(54), 
        Array("-td", "build/"),
        firtoolOpts = firtool_options,
    )
}

object Cache_Main extends App {
    var firtool_options = Array("-disable-all-randomization", 
                                "-strip-debug-info",
                                "-strip-fir-debug-info",
                                "-O=release",
                                "--ignore-read-enable-mem",
                                "--lower-memories",
                                "--lowering-options=disallowLocalVariables, explicitBitcast, disallowMuxInlining, disallowExpressionInliningInPorts, verifLabels",
                                "-o=verilog/",
                                "-split-verilog",
                                //"--scalarize-top-module"
                                
                                )
    if(System.getProperties().getProperty("mode") != "sim"){
        firtool_options = firtool_options ++ Array("--vb-to-bv")
    }
    
    ChiselStage.emitSystemVerilogFile(
        new Dcache, 
        Array("-td", "build/"),
        firtoolOpts = firtool_options,
    )
}


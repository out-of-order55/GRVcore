package grvcore.common
import chisel3._
import java.io.{PrintWriter, File}

object SimTable{

	def createRTTable(): Seq[(UInt, String)] = {
        Seq(
            (RT_FIX, "RT_FIX"),
            (RT_FLT, "RT_FLT"),
            (RT_PAS, "RT_PAS"),
            (RT_X, "RT_X")
        )
	}
	def createIQTable(): Seq[(UInt, String)] = {
        Seq(
            (IQT_INT, "IQT_INT"),
            (IQT_MEM, "IQT_MEM"),
            (IQT_FP , "IQT_FP"),
            (IQT_MFP, "IQT_MFP")
        )
	}

	def createImmTable(): Seq[(UInt, String)] = {
        Seq(
            (IS_I,"IS_I"),
            (IS_S,"IS_S"),
            (IS_B,"IS_B"),
            (IS_U,"IS_U"),
            (IS_J,"IS_J")
        )
	}

	def createFUTable(): Seq[(UInt, String)] = {
        Seq(
            (FU_ALU,"FU_ALU"),
            (FU_JMP,"FU_JMP"),
            (FU_MEM,"FU_MEM"),
            (FU_MUL,"FU_MUL"),
            (FU_DIV,"FU_DIV"),
            (FU_CSR,"FU_CSR"),
            (FU_FPU,"FU_FPU"),
            (FU_FDV,"FU_FDV"),
            (FU_I2F,"FU_I2F"),
            (FU_F2I,"FU_F2I")
        )
	}
	def createUOPTable(): Seq[(UInt, String)] = {
        Seq(
            (uopNOP  	,"uopNOP"),          
            (uopLD   	,"uopLD"),          
            (uopSTA  	,"uopSTA"),          
            (uopSTD  	,"uopSTD"),          
            (uopLUI  	,"uopLUI"),          
            (uopADDI 	,"uopADDI"),          
            (uopANDI 	,"uopANDI"),          
            (uopORI  	,"uopORI"),          
            (uopXORI 	,"uopXORI"),          
            (uopSLTI 	,"uopSLTI"),          
            (uopSLTIU	,"uopSLTIU"),          
            (uopSLLI 	,"uopSLLI"),          
            (uopSRAI 	,"uopSRAI"),          
            (uopSRLI 	,"uopSRLI"),          
            (uopSLL  	,"uopSLL"),          
            (uopADD  	,"uopADD"),          
            (uopSUB  	,"uopSUB"),          
            (uopSLT  	,"uopSLT"),          
            (uopSLTU 	,"uopSLTU"),          
            (uopAND  	,"uopAND"),          
            (uopOR   	,"uopOR"),          
            (uopXOR  	,"uopXOR"),          
            (uopSRA  	,"uopSRA"),          
            (uopSRL  	,"uopSRL"),          
            (uopBEQ  	,"uopBEQ"),          
            (uopBNE  	,"uopBNE"),          
            (uopBGE  	,"uopBGE"),          
            (uopBGEU 	,"uopBGEU"),          
            (uopBLT  	,"uopBLT"),          
            (uopBLTU 	,"uopBLTU"),          
            (uopCSRRW	,"uopCSRRW"),          
            (uopCSRRS	,"uopCSRRS"),          
            (uopCSRRC	,"uopCSRRC"),          
            (uopCSRRWI  ,"uopCSRRWI"),        	
            (uopCSRRSI  ,"uopCSRRSI"),        	
            (uopCSRRCI  ,"uopCSRRCI"),        	
            (uopJ   	,"uopJ"),           	
            (uopJAL 	,"uopJAL"),           	
            (uopJALR 	,"uopJALR"),          
            (uopAUIPC	,"uopAUIPC"),          
            (uopCFLSH	,"uopCFLSH"),          
            (uopFENCE	,"uopFENCE"),          
            (uopMUL  	,"uopMUL"),          
            (uopMULH 	,"uopMULH"),          
            (uopMULHU	,"uopMULHU"),          
            (uopMULHSU  ,"uopMULHSU"),        	
            (uopDIV  	,"uopDIV"),          
            (uopDIVU 	,"uopDIVU"),          
            (uopREM  	,"uopREM"),          
            (uopREMU 	,"uopREMU"),          
            (uopFENCEI  ,"uopFENCEI"),         
            (uopAMO_AG  ,"uopAMO_AG"),         
            (uopWFI     ,"uopWFI"),         
            (uopERET    ,"uopERET"),         
            (uopSFENCE  ,"uopSFENCE")         
        )
	}
    def outputTableToFile(filename: String, table: Seq[(UInt, String)]): Unit = {
        val writer = new PrintWriter(new File(filename))

        table.foreach { case (value, name) =>
            val newVal = value.litValue
            val bitWidth = value.getWidth  // 获取位宽

            // // 根据位宽判断使用二进制还是十六进制输出
            // if (bitWidth < 4) {
            // // 位宽小于 4，使用二进制格式
            //     writer.println(String.format(s"%${bitWidth}s %s", newVal.toString(2).padTo(bitWidth, '0'), name))
            // } else {
            //     val hexDigits = (bitWidth + 3) / 4
            //     val newValLong = newVal.toLong
            //     val hexFormat = s"%0${hexDigits}x"
            //     writer.println(String.format(hexFormat, newValLong) + s" $name")
            // }
                val hexDigits = (bitWidth + 3) / 4
                val newValLong = newVal.toLong
                val hexFormat = s"%0${hexDigits}x"
                writer.println(String.format(hexFormat, newValLong) + s" $name")
        }

        writer.close()
    }

	def printLookupTables(): Unit = {
		// 输出到不同的文件
		outputTableToFile("./build/rttable.txt", createRTTable())
        outputTableToFile("./build/uoptable.txt", createUOPTable())
        outputTableToFile("./build/futable.txt", createFUTable())
        outputTableToFile("./build/iqtable.txt", createIQTable())
		outputTableToFile("./build/immtable.txt", createImmTable())
	}
}
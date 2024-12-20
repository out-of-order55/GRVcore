package grvcore

import chisel3._
import chisel3.util._
import grvcore.common._
import org.chipsalliance.cde.config.Parameters
case class IssueParams(
    dispatchWidth: Int = 1,
    issueWidth: Int = 1,
    numEntries: Int = 8,
    iqType: BigInt
)
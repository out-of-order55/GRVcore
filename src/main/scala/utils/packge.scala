package grvcore

package object common extends
    grvcore.GRVOpConstants with
    grvcore.RISCVConstants with 
    freechips.rocketchip.rocket.constants.ScalarOpConstants
    with freechips.rocketchip.rocket.constants.MemoryOpConstants
{
}
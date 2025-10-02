// Clk divider. Initiallyl  written by Marko Kosunen
// Divides input clock by N, 2N , 4N and 8N
// Last modification by Marko Kosunen, marko.kosunen@aalto.fi, 31.10.2018 13:49
package clkdiv_universal

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import dsptools._
import dsptools.numbers._
import breeze.math.Complex

class clkdiv_universalCTRL(n: Int, word_res: Int=32, out_res: Int=16) extends Bundle {
    val Ndiv       = Input(UInt(n.W))
    val reset_clk  = Input(Bool())
    val shift      = Input(UInt(3.W))
    val convmode   = Input(UInt(1.W))
    val word       = Input(UInt(word_res.W))
    val word_mu    = Input(UInt(word_res.W))
}

class clkdiv_universalIO(n: Int, word_res: Int=32, out_res: Int=16) extends Bundle {
    val control = new clkdiv_universalCTRL(n=n)
    val out = new Bundle {
        val clkpfn      = Output(Bool())
        val clkpf2n     = Output(Bool())
        val clkpf4n     = Output(Bool())
        val clkpf8n     = Output(Bool())
        val clkpn       = Output(Bool())
        val clk_slowest = Output(Bool())
        val clkpn_sync  = Output(Bool())
        val phase       = Output(SInt((out_res).W))
    }
}

class clkdiv_universal (n: Int=8, word_res: Int=32, out_res: Int=16) extends Module {
    val io = IO(new clkdiv_universalIO(n=n))

    val en = Wire(Bool()) 
    en := !io.control.reset_clk 
    
    val r_shift        = RegInit(0.U.asTypeOf(io.control.shift))
    val r_Ndiv         = RegInit(1.U.asTypeOf(io.control.Ndiv))
    //Sync the shift
    r_shift := io.control.shift

    //Sync the Ndiv
    r_Ndiv := io.control.Ndiv

    val div_n=RegInit(false.B)
    val count          = RegInit(0.U(n.W))
    val phase_reset_primary =RegInit(true.B)
    val phase_reset_secondary =RegInit(true.B)
    phase_reset_secondary := ShiftRegister(phase_reset_primary, 5, true.B, true.B)
    when (en) {
        when (count >= r_Ndiv - 1) {
            count := 0.U
            div_n := true.B
            phase_reset_primary := false.B
        } .otherwise {
            count := count + 1.U(1.W)
            div_n := false.B
            phase_reset_primary :=phase_reset_primary
        }
    }

    
    val neg_clock = (!(clock.asBool)).asClock
    val clk_div_n_mux= Wire(Bool())
    val use_div_n = withClock(neg_clock){RegInit(false.B)}
    use_div_n :=withClock(neg_clock){r_shift >= 1.U  }
    clk_div_n_mux := Mux(use_div_n.asBool, clock.asBool, div_n)
    io.out.clkpn  := clk_div_n_mux


    // Phase accum and register for mu
      val phase_reset =RegInit(true.B)
      val phaseaccum =  withClockAndReset(clk_div_n_mux.asClock, phase_reset_secondary){Module(new phaseaccum(word_res=word_res,out_res=out_res)) }
      phaseaccum.io.control.word     := io.control.word
      phaseaccum.io.control.word_mu  := io.control.word_mu
      phaseaccum.io.control.convmode := io.control.convmode

      io.out.clkpn_sync  := phaseaccum.io.out.clkp1_sync
      io.out.phase := phaseaccum.io.out.phase


    val enab_frac            = withClock(neg_clock){RegInit(0.U(1.W))}
    val convmode_switch_async= withClock(neg_clock){RegInit(0.U(1.W))}

    convmode_switch_async   := withClock(neg_clock){io.control.convmode}
    enab_frac           := withClock(neg_clock){io.control.word =/= 0.U}
    val clk_div_master_mux= Wire(Bool())
    clk_div_master_mux := Mux(enab_frac.asBool, phaseaccum.io.out.clkpf, clock.asBool)
    io.out.clkpfn  := clk_div_master_mux

        
    withClock(clk_div_master_mux.asClock){
      val stateregisters = RegInit(VecInit(Seq.fill(3)(false.B)))

      

      val enN = RegInit(false.B) 
      enN := en

      //Enable registers. We need to delay the enable by one clock pulse in order
      // to get the feedbacks reseted
      val en2 = RegInit(false.B)
      val en4 = RegInit(false.B)
      val en8 = RegInit(false.B)
      
      stateregisters(0) := ~stateregisters(0)
      //Chaining the enables
      when (stateregisters(0)){ 
          en2 := (enN &&  en )
      }
      when (stateregisters(1) ){
          en4 := en2
      }
      when (stateregisters(2) ){
          en8 := en4
      }
      
      val enchain = Seq(enN, en2, en4, en8)


      // Monitors if the all previous stages are zero
      val allzp = Wire(Vec(3,Bool()))
      allzp(0) :=  stateregisters(0) 

      for ( i <- 1 to 2) {
          allzp(i) := allzp(i - 1) && !stateregisters(i)
      }

      val outregs = RegInit(VecInit(Seq.fill(3)(false.B)))
      
      outregs(0) := stateregisters(0)

      for ( i <- 1 to 2) {
         when (en) { 
             when ((enchain(i) && allzp(i - 1))) {
                stateregisters(i) := ! stateregisters(i)
             } .otherwise { 
                 stateregisters(i) := stateregisters(i)
             }
         } .otherwise { 
             stateregisters(i) := false.B  
       }
       //Pure registers at the output
       outregs(i) := stateregisters(i)
      }

      
      //First we sync or zero the divided clocks depending on the shift 
      val syncregs = RegInit(VecInit(Seq.fill(3)(false.B)))
      val w_clkpn = Wire(Bool())
  
      //Shifting mux
      w_clkpn := RegNext(outregs(0))
      when (r_shift === 0.U){
          syncregs(0) := w_clkpn
          syncregs(1) := outregs(1) 
          syncregs(2) := outregs(2)
          //syncregs(3) := outregs(3)
      }.elsewhen(r_shift - 1 === 0.U){
          syncregs(0) := w_clkpn
          syncregs(1) := outregs(1) 
          syncregs(2) := outregs(2)
          //syncregs(3) := outregs(2)
      }.elsewhen(r_shift - 2 === 0.U){
          syncregs(0) := 0.U;
          syncregs(1) := w_clkpn 
          syncregs(2) := outregs(1)
          //syncregs(3) := outregs(1)
      }.elsewhen(r_shift - 3 === 0.U){
          syncregs(0) := 0.U
          syncregs(1) := 0.U
          syncregs(2) := w_clkpn
          //syncregs(3) := w_clkpn
      }.elsewhen(r_shift - 4 === 0.U){
          syncregs(0) := 0.U
          syncregs(1) := 0.U 
          syncregs(2) := 0.U
          //syncregs(3) := 0.U
      }.otherwise{
          syncregs(0) := w_clkpn
          syncregs(1) := outregs(1) 
          syncregs(2) := outregs(2)
          //syncregs(3) := outregs(3)
      }


      // Output selection logic
      val w_isdivone = Wire(Bool())
      w_isdivone := (r_Ndiv - 1 === 0.U)

      val w_sel1_clock_clkpfn  = Wire(Bool())
      val w_sel1_clock_clkpf2n = Wire(Bool())
      val w_sel1_clock_clkpf4n = Wire(Bool())
      val w_sel1_clock_clkpf8n = Wire(Bool())
      val w_seln_clock_clkpfn  = Wire(Bool())
      val w_seln_clock_clkpf2n = Wire(Bool())
      val w_seln_clock_clkpf4n = Wire(Bool())
      val w_seln_clock_clkpf8n = Wire(Bool()) 

      //Selector signals for the output mux
      w_sel1_clock_clkpfn  := w_isdivone && ((r_shift === 0.U))
      w_sel1_clock_clkpf2n := w_isdivone && ((r_shift === 0.U) || (r_shift - 1 === 0.U))
      w_sel1_clock_clkpf4n := w_isdivone && ((r_shift === 0.U) || (r_shift - 1 === 0.U) || (r_shift - 2 === 0.U))
      w_sel1_clock_clkpf8n := w_isdivone && ((r_shift === 0.U) || (r_shift - 1 === 0.U) || (r_shift - 2 === 0.U) || (r_shift - 3 === 0.U))
      
      w_seln_clock_clkpfn  := ((r_shift - 1 === 0.U))
      w_seln_clock_clkpf2n := ((r_shift - 2 === 0.U))
      w_seln_clock_clkpf4n := ((r_shift - 3 === 0.U))
      w_seln_clock_clkpf8n := ((r_shift - 4 === 0.U))

      // Output Muxes
      //Mux for clkpfn
      when (w_sel1_clock_clkpfn || w_seln_clock_clkpfn){
          io.out.clkpfn := clk_div_master_mux.asUInt
      } .otherwise {
          io.out.clkpfn := clk_div_master_mux
      }
      //Mux for clkp2n
      when (w_sel1_clock_clkpf2n || w_seln_clock_clkpf2n){
          io.out.clkpf2n := clk_div_master_mux.asUInt
      } .otherwise {
          io.out.clkpf2n := syncregs(0)
      }
      //Mux for clkp4n
      when (w_sel1_clock_clkpf4n || w_seln_clock_clkpf4n){
          io.out.clkpf4n := clk_div_master_mux.asUInt
      } .otherwise {
          io.out.clkpf4n := syncregs(1)
      }
      //Mux for clkp8n
      when (w_sel1_clock_clkpf8n || w_seln_clock_clkpf8n){
          io.out.clkpf8n := clk_div_master_mux.asUInt
      } .otherwise {
          io.out.clkpf8n := syncregs(2)
      }

       io.out.clk_slowest := syncregs(2)
  }
}

class phaseaccumIO (word_res: Int=32, out_res :Int=16) extends Bundle {
  val control = new Bundle {
    val word = Input(UInt(word_res.W))
    val word_mu = Input(UInt(word_res.W))
    val convmode = Input(UInt(1.W))
  }
  val out = new Bundle {
    val phase = Output(SInt((out_res).W))
    val clkpf       = Output(Bool())
    val clkp1_sync  = Output(Bool())
  }
}

class phaseaccum (word_res: Int=32, out_res: Int=16) extends Module {
  val io = IO(new phaseaccumIO(word_res=word_res, out_res=out_res))
  val neg_clock = (!(clock.asBool)).asClock

  val accum           = RegInit(0.U((word_res+1).W))
  val word_reg        = RegInit(0.U(word_res.W))
  val word_mu_reg     = RegInit(0.U(word_res.W))
  val accum_mu        = RegInit(0.U((word_res+1).W))
  val out_reg         = RegInit(0.S(out_res.W))
  val enab            = withClock((!(clock.asBool)).asClock){RegInit(0.U(1.W))}
  val enab_count      = withClock((!(clock.asBool)).asClock){RegInit(0.U(1.W))}
  val enab2           = withClock((!(clock.asBool)).asClock){RegInit(0.U(1.W))}
  val enab_clk_sync   = withClock((!(clock.asBool)).asClock){RegInit(0.U(1.W))}
  val enab2_re        = RegInit(0.U(1.W))
  val accum_msb       = RegInit(0.U(1.W))
  
  word_reg        := ShiftRegister(io.control.word, 1, 0.U, true.B)
  word_mu_reg     := ShiftRegister(io.control.word_mu, 1, 0.U, true.B)
  enab2           := withClock(neg_clock){io.control.word =/= 0.U} //& io.control.convmode===0.U
  enab_clk_sync   := withClock(neg_clock){ShiftRegister(enab2, 4, 0.U, true.B)} 
  enab2_re        := enab2 & !ShiftRegister(enab2, 1, 0.U, true.B)

  when(io.control.convmode === 1.U){
    enab2_re := ShiftRegister(enab2 & !ShiftRegister(enab2, 1, 0.U, true.B), 2, 0.U, true.B)
  }.otherwise {
    enab2_re := enab2 & !ShiftRegister(enab2, 1, 0.U, true.B)
  }
  
  accum           := accum(word_res, 0) +& word_reg
  accum_msb       := accum(word_res)
  enab            := withClock(neg_clock){ShiftRegister(((accum(word_res) =/= accum_msb) | enab2_re), 2, 0.U, true.B) }
  enab_count      := withClock(neg_clock){(accum(word_res) =/= accum_msb) | enab2_re }
  io.out.clkpf     := clock.asUInt & enab & enab2
  io.out.clkp1_sync := clock.asUInt & enab & enab2
  out_reg         := accum((word_res-1), (word_res-out_res+1)).zext
  io.out.phase    := out_reg 

  when(enab_count.asBool){
    accum_mu := accum_mu(word_res-1, 0) +& (word_mu_reg)
  }

  when (io.control.convmode === 1.U){
    out_reg           := accum_mu((word_res-1), (word_res-out_res+1)).zext
    io.out.clkp1_sync       := clock.asUInt & enab_clk_sync
    io.out.clkpf   := clock.asUInt & enab & enab_clk_sync
    io.out.phase      := ShiftRegister(out_reg, 1, 0.S, true.B)  
  }.otherwise {
    io.out.clkpf      := clock.asUInt & enab
    io.out.clkp1_sync := clock.asUInt & enab_clk_sync
    out_reg           := accum((word_res-1), (word_res-out_res+1)).zext
    io.out.phase      := out_reg
  }
}

//This gives you verilog


object clkdiv_universal extends App {
//   // Generate verilog
    val annos = Seq(ChiselGeneratorAnnotation(() => new clkdiv_universal(n=8,word_res=16,out_res=16)))
    val sysverilog = (new ChiselStage).emitSystemVerilog(
        new clkdiv_universal(n=8,word_res=16, out_res=16))
}



// See LICENSE for license details.
// Use handlebars for template generation
//
//Start with a static tb and try to genererate a gnerator for it
// This uses clkdiv_n_2_4_8 verilog. You need to compile it separately

package f2_dsp_tapein5

import chisel3._
import java.io.{File, FileWriter, BufferedWriter}
import com.gilt.handlebars.scala.binding.dynamic._
import com.gilt.handlebars.scala.Handlebars

//Testbench.
object tb_f2_dsp_tapein5 {
    def extpargen(): String={
        val extpars=Seq(("g_infile","\"./A.txt\""), 
                        ("g_outfile","\"./Z.txt\""),
                        ("g_Rs_high","16*8*20.0e6"),
                        ("g_Rs_low ","20.0e6"),
                        ("g_scale0 ","1"),
                        ("g_scale1 ","1"),
                        ("g_scale2 ","1"),
                        ("g_scale3 ","1"),
                        ("g_mode   ","4"),
                        ("g_user_index","0"),
                        ("g_antenna_index","0"),
                        ("g_rx_output_mode","0"),
                        ("g_input_mode","0"),
                        ("g_mode   ","4"))

        var externalpars=("//Things you want to control from the simulator cmdline must be parameters \nmodule {{oname}} #("
                                +extpars.map{ case (par,value) => "parameter %s = %s,\n            ".format(par,value)}.mkString)
        externalpars=externalpars.patch(externalpars.lastIndexOf(','),"",1)+");"
         externalpars
    }

    def main(args: Array[String]): Unit = {
        val name= this.getClass.getSimpleName.split("\\$").last
        val tb = new BufferedWriter(new FileWriter("./verilog/"+name+".v"))
        object tbvars {
          val oname=name
          val dutmod = "f2_dsp_tapein5" 
          val n = 16
          val resolution=32
          val ulimit=resolution-n-1
          val gainbits= 10
          val gainlimit=gainbits-1
          val clk0="decimator_controls_0_cic3clockslow"
          val clk1="decimator_controls_0_hb1clock_low"
          val clk2="decimator_controls_0_hb2clock_low"
          val clk3="decimator_controls_0_hb3clock_low"
          val clk4="clock_symrate"
          val clk5="clock_symratex4"
          val clk6="clock_outfifo_deq"
          val clk7="clock_infifo_enq"
          val sig0="decimator_controls_0_cic3integscale"
          val sig1="decimator_controls_0_hb1scale"
          val sig2="decimator_controls_0_hb2scale"
          val sig3="decimator_controls_0_hb3scale"
          val sig4="decimator_controls_0_mode"
          val sig4limit=2
          val sig5="decimator_controls_1_cic3integscale"
          val sig6="decimator_controls_1_hb1scale"
          val sig7="decimator_controls_1_hb2scale"
          val sig8="decimator_controls_1_hb3scale"
          val sig9="decimator_controls_1_mode"
          val sig10="decimator_controls_2_cic3integscale"
          val sig11="decimator_controls_2_hb1scale"
          val sig12="decimator_controls_2_hb2scale"
          val sig13="decimator_controls_2_hb3scale"
          val sig14="decimator_controls_2_mode"
          val sig15="decimator_controls_3_cic3integscale"
          val sig16="decimator_controls_3_hb1scale"
          val sig17="decimator_controls_3_hb2scale"
          val sig18="decimator_controls_3_hb3scale"
          val sig19="decimator_controls_3_mode"
          val sig20="user_index"
          val sig21="antenna_index"
          val indexlimit=1
          val sig22="reset_index_count"
          val sig23="reset_outfifo"
          val sig24="reset_infifo"
          val sig25="rx_output_mode"
          val sig26="input_mode"
          val sig27="reset_infifo"
          val sig28="iptr_A_0_real"
          val sig29="iptr_A_0_imag"
          val sig30="iptr_A_1_real"
          val sig31="iptr_A_1_imag"
          val sig32="iptr_A_2_real"
          val sig33="iptr_A_2_imag"
          val sig34="iptr_A_3_real"
          val sig35="iptr_A_3_imag"
        }
        val header="//This is a tesbench generated with scala generator\n"
        val extpars=extpargen()
        val textTemplate=header+ extpars+"""
                        |//timescale 1ps this should probably be a global model parameter 
                        |parameter integer c_Ts=1/(g_Rs_high*1e-12);
                        |parameter integer c_ratio0=g_Rs_high/(8*g_Rs_low);
                        |parameter integer c_ratio1=g_Rs_high/(4*g_Rs_low);
                        |parameter integer c_ratio2=g_Rs_high/(2*g_Rs_low);
                        |parameter integer c_ratio3=g_Rs_high/(g_Rs_low);
                        |parameter RESET_TIME = 5*c_Ts;
                        |
                        |//These registers always needed
                        |reg clock;
                        |reg reset;
                        |
                        |//register to set the clock division ratio
                        |reg [7:0] io_Ndiv;
                        |reg io_reset_clk;
                        |
                        |//Registers for inputs
                        |reg signed [{{ulimit}}:0] io_{{sig28}} =0;
                        |reg signed [{{ulimit}}:0] io_{{sig28}} =0;
                        |reg signed [{{ulimit}}:0] io_{{sig30}} =0;
                        |reg signed [{{ulimit}}:0] io_{{sig31}} =0;
                        |reg signed [{{ulimit}}:0] io_{{sig32}} =0;
                        |reg signed [{{ulimit}}:0] io_{{sig33}} =0;
                        |reg signed [{{ulimit}}:0] io_{{sig34}} =0;
                        |reg signed [{{ulimit}}:0] io_{{sig35}} =0;
                        |reg signed [{{gainlimit}}:0] io_{{sig0}};
                        |reg signed [{{gainlimit}}:0] io_{{sig1}};
                        |reg signed [{{gainlimit}}:0] io_{{sig2}};
                        |reg signed [{{gainlimit}}:0] io_{{sig3}};
                        |reg signed [{{sig4limit}}:0] io_{{sig4}};
                        |reg signed [{{gainlimit}}:0] io_{{sig5}};
                        |reg signed [{{gainlimit}}:0] io_{{sig6}};
                        |reg signed [{{gainlimit}}:0] io_{{sig7}};
                        |reg signed [{{gainlimit}}:0] io_{{sig8}};
                        |reg signed [{{sig4limit}}:0] io_{{sig9}};
                        |reg signed [{{gainlimit}}:0] io_{{sig10}};
                        |reg signed [{{gainlimit}}:0] io_{{sig11}};
                        |reg signed [{{gainlimit}}:0] io_{{sig12}};
                        |reg signed [{{gainlimit}}:0] io_{{sig13}};
                        |reg signed [{{sig4limit}}:0] io_{{sig14}};
                        |reg signed [{{gainlimit}}:0] io_{{sig15}};
                        |reg signed [{{gainlimit}}:0] io_{{sig16}};
                        |reg signed [{{gainlimit}}:0] io_{{sig17}};
                        |reg signed [{{gainlimit}}:0] io_{{sig18}};
                        |reg signed [{{sig4limit}}:0] io_{{sig19}};
                        |reg unsigned [{{indexlimit}}:0] io_{{sig20}};
                        |reg unsigned [{{indexlimit}}:0] io_{{sig21}};
                        |
                        |//Additional resets
                        |reg io_{{sig22}};
                        |reg io_{{sig23}};
                        |reg io_{{sig24}};
                        |
                        |//Modes
                        |reg unsigned io_{{sig25}};
                        |reg unsigned io_{{sig26}};
                        |
                        |//Wires for outputs
                        |wire signed [{{ulimit}}:0] io_Z_0_real;
                        |wire signed [{{ulimit}}:0] io_Z_0_imag;
                        |wire signed [{{ulimit}}:0] io_Z_1_real;
                        |wire signed [{{ulimit}}:0] io_Z_1_imag;
                        |wire signed [{{ulimit}}:0] io_Z_2_real;
                        |wire signed [{{ulimit}}:0] io_Z_2_imag;
                        |wire signed [{{ulimit}}:0] io_Z_3_real;
                        |wire signed [{{ulimit}}:0] io_Z_3_imag;
                        |
                        |//Wires for additional clocks
                        |wire io_{{clk0}};
                        |wire io_{{clk1}};
                        |wire io_{{clk2}};
                        |wire io_{{clk3}};
                        |wire io_{{clk4}};
                        |wire io_{{clk5}};
                        |wire io_{{clk6}};
                        |wire io_{{clk7}};
                        |
                        |//File IO parameters
                        |integer StatusI, StatusO, infile, outfile;
                        |integer count0;
                        |integer count1;
                        |integer count2;
                        |integer count3;
                        |integer din1,din2;
                        |
                        |//Initializations
                        |initial count0 = 0;
                        |initial count1 = 0;
                        |initial count2 = 0;
                        |initial count3 = 0;
                        |initial clock = 1'b0;
                        |initial reset = 1'b0;
                        |initial outfile = $fopen(g_outfile,"w"); // For writing
                        |
                        |//Clock definitions
                        |always #(c_Ts)clock = !clock ;
                        | 
                        |//always @(posedge io_{{clk0}}) begin 
                        |//always @(posedge io_{{clk1}}) begin 
                        |//always @(posedge io_{{clk2}}) begin 
                        |always @(posedge io_{{clk3}}) begin 
                        |    //Print only valid values 
                        |    if (~($isunknown( io_Z_0_real)) &&   ~($isunknown( io_Z_0_imag)) && ~($isunknown( io_Z_1_real)) && ~($isunknown( io_Z_1_imag))) begin
                        |        $fwrite(outfile, "%d\t%d\t%d\t%d\n", io_Z_0_real, io_Z_0_imag, io_Z_1_real, io_Z_1_imag);
                        |    end
                        |    else begin
                        |        $fwrite(outfile, "%d\t%d\t%d\t%d\n",0,0,0,0);
                        |    end 
                        |end
                        |
                        |//Clock divider model
                        |clkdiv_n_2_4_8 clockdiv( // @[:@3.2]
                        |  .clock(clock), // @[:@4.4]
                        |  .reset(reset), // @[:@5.4]
                        |  .io_Ndiv(io_Ndiv), // @[:@6.4]
                        |  .io_reset_clk(io_reset_clk), // @[:@6.4]
                        |  .io_clkpn (io_{{clk0}}), // @[:@6.4]
                        |  .io_clkp2n(io_{{clk1}}), // @[:@6.4]
                        |  .io_clkp4n(io_{{clk2}}), // @[:@6.4]
                        |  .io_clkp8n(io_{{clk3}})// @[:@6.4]
                        |);
                        |
                        |//DUT definition
                        |{{dutmod}} DUT ( // @[:@3740.2]
                        |    .clock(clock), // @[:@3741.4]
                        |    .reset(reset), // @[:@3742.4]
                        |    .io_{{clk0}}(io_{{clk0}}), // @[:@3743.4]
                        |    .io_{{clk1}}(io_{{clk1}}), // @[:@3743.4]
                        |    .io_{{clk2}}(io_{{clk2}}), // @[:@3743.4]
                        |    .io_{{clk3}}(io_{{clk3}}), // @[:@3743.4]
                        |    .io_{{clk4}}(io_{{clk0}}), // @[:@3743.4]
                        |    .io_{{clk5}}(io_{{clk1}}), // @[:@3743.4]
                        |    .io_{{clk6}}(io_{{clk2}}), // @[:@3743.4]
                        |    .io_{{clk7}}(io_{{clk3}}), // @[:@3743.4]
                        |    .io_{{sig0}}(io_{{sig0}}), // @[:@3743.4]
                        |    .io_{{sig1}}(io_{{sig1}}), // @[:@3743.4]
                        |    .io_{{sig2}}(io_{{sig2}}), // @[:@3743.4]
                        |    .io_{{sig3}}(io_{{sig3}}), // @[:@3743.4]
                        |    .io_{{sig4}}(io_{{sig4}}), // @[:@3743.4]
                        |    .io_{{sig5}}(io_{{sig8}}), // @[:@3743.4]
                        |    .io_{{sig6}}(io_{{sig6}}), // @[:@3743.4]
                        |    .io_{{sig7}}(io_{{sig7}}), // @[:@3743.4]
                        |    .io_{{sig8}}(io_{{sig8}}), // @[:@3743.4]
                        |    .io_{{sig9}}(io_{{sig9}}), // @[:@3743.4]
                        |    .io_{{sig10}}(io_{{sig10}}), // @[:@3743.4]
                        |    .io_{{sig11}}(io_{{sig11}}), // @[:@3743.4]
                        |    .io_{{sig12}}(io_{{sig12}}), // @[:@3743.4]
                        |    .io_{{sig13}}(io_{{sig13}}), // @[:@3743.4]
                        |    .io_{{sig14}}(io_{{sig14}}), // @[:@3743.4]
                        |    .io_{{sig15}}(io_{{sig18}}), // @[:@3743.4]
                        |    .io_{{sig16}}(io_{{sig16}}), // @[:@3743.4]
                        |    .io_{{sig17}}(io_{{sig17}}), // @[:@3743.4]
                        |    .io_{{sig18}}(io_{{sig18}}), // @[:@3743.4]
                        |    .io_{{sig19}}(io_{{sig19}}), // @[:@3743.4]
                        |    .io_iptr_A_real, // @[:@3743.4]
                        |    .io_iptr_A_imag, // @[:@3743.4]
                        |    .io_Z_0_real, // @[:@3743.4]
                        |    .io_Z_0_imag, // @[:@3743.4]
                        |    .io_Z_1_real, // @[:@3743.4]
                        |    .io_Z_1_imag // @[:@3743.4]
                        |   );
                        |
                        |initial #0 begin
                        |    io_{{sig0}} = g_scale0;
                        |    io_{{sig1}} = g_scale1;
                        |    io_{{sig2}} = g_scale2;
                        |    io_{{sig3}} = g_scale3;
                        |    io_{{sig4}} = g_mode;
                        |    io_{{sig5}} = g_scale0;
                        |    io_{{sig6}} = g_scale1;
                        |    io_{{sig7}} = g_scale2;
                        |    io_{{sig8}} = g_scale3;
                        |    io_{{sig9}} = g_mode;
                        |    io_{{sig10}} = g_scale0;
                        |    io_{{sig11}} = g_scale1;
                        |    io_{{sig12}} = g_scale2;
                        |    io_{{sig13}} = g_scale3;
                        |    io_{{sig14}} = g_mode;
                        |    io_{{sig15}} = g_scale0;
                        |    io_{{sig16}} = g_scale1;
                        |    io_{{sig17}} = g_scale2;
                        |    io_{{sig18}} = g_scale3;
                        |    io_{{sig19}} = g_mode;
                        |    io_{{sig20}} = g_user_index;
                        |    io_{{sig21}} = g_antenna_index;
                        |    io_Ndiv= c_ratio0;
                        |    //Resets
                        |    reset=1;
                        |    io_reset_clk=1;
                        |    io_{{sig21}} = 1;
                        |    io_{{sig23}} = 1;
                        |    io_{{sig24}} = 1;
                        |    #RESET_TIME
                        |    reset=0;
                        |    io_reset_clk=0;
                        |    io_{{sig21}} = 0;
                        |    io_{{sig23}} = 0;
                        |    io_{{sig24}} = 0;
                        |
                        |    infile = $fopen(g_infile,"r"); // For reading
                        |    while (!$feof(infile)) begin
                        |            @(posedge clock) 
                        |             StatusI=$fscanf(infile, "%d\t%d\n", din1, din2);
                        |             io_iptr_A_real <= din1;
                        |             io_iptr_A_imag <= din2;
                        |    end
                        |    $fclose(infile);
                        |    $fclose(outfile);
                        |    $finish;
                        |end
                        |endmodule""".stripMargin('|')
        val testbench=Handlebars(textTemplate)
        tb write testbench(tbvars)
        tb.close()
  }
}

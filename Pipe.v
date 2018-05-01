// Basic
`define WORD      [15:0]
`define OpCode      [15:12]
`define Dest      [11:8]
`define Sc        [7:4]
`define alt       [3:0]
`define Imm       [7:0]
`define EN        [31:0]
`define STATE     [3:0]
`define Op        [4:0]

// Size references
`define REGSIZE     [15:0]
`define MEMSIZE     [65535:0]
`define RETADDR     [63:0]
`define REGNAME     [3:0]
`define ESTACKSIZE    [31:0]
`define CSTACKSIZE    [63:0]

// 4bit Operatrors
`define OpAdd   4'b0000
`define OpAnd   4'b0001
`define OpMul   4'b0010
`define OpOr    4'b0011
`define OpSll   4'b0100
`define OpSlt   4'b0101
`define OpSra   4'b0110
`define OpXor   4'b0111
`define OpGor   4'b1000
`define OpLeft   4'b1001
`define OpRight  4'b1010
`define OpLnot   4'b1011
`define OpNeg   4'b1100
`define OpLI8   4'b1101
`define OpLU8   4'b1110
`define OpSc    4'b1111

// 5bit extended Operatrors  
`define OpLoad    5'b10000
`define OpStore   5'b10001
`define OpAllen   5'b10010
`define OpPOpen   5'b10011
`define OpPushen  5'b10100
`define OpRet     5'b10101
`define OpNOp     5'b10110
`define OpTrap    5'b10111
`define OpCall    5'b11000
`define OpJump    5'b11001
`define OpJumpf   5'b11010


//Source Codes
`define ScLoad    4'b0000
`define ScStore   4'b0001
`define ScAllen   4'b0010
`define ScPOpen   4'b0011
`define ScPushen  4'b0100
`define ScRet   4'b0101
`define ScNOp   4'b0110
`define ScTrap    4'b0111
`define ScCall    4'b1000
`define ScJump    4'b1001
`define ScJumpf   4'b1010

module decode(Opout, regd, in, ir);
output reg `Op Opout;
output reg `REGNAME regd;

input wire `Op in;
input `WORD ir;

always @(in, ir) begin
  if((in == `OpJumpf) || (in == `OpJump) || (in == `OpCall)) begin
    Opout = `OpNOp;  // 2nd word of li becomes nOp
    regd = 0;           // No writing will occur
  end else begin
    case (ir `OpCode)
      `OpSc:begin
      regd=0;
      case (ir `Dest)
          `ScLoad:    Opout = `OpLoad;
          `ScStore:   Opout = `OpStore;
          `ScAllen:   Opout = `OpAllen;
          `ScPOpen:   Opout = `OpPOpen;
          `ScPushen:  Opout = `OpPushen;
          `ScRet:     Opout = `OpRet;
          `ScNOp:     Opout = `OpNOp;
          `ScTrap:    Opout = `OpTrap;
          `ScCall:    Opout = `OpCall;
          `ScJump:    Opout = `OpJump;
          `ScJumpf:   Opout = `OpJumpf;
        endcase
        end
        `OpAdd: begin Opout = ir `OpCode; regd = ir `Dest; end
        `OpAnd: begin Opout = ir `OpCode; regd = ir `Dest; end
        `OpMul: begin Opout = ir `OpCode; regd = ir `Dest; end
        `OpOr: begin Opout = ir `OpCode; regd = ir `Dest; end
        `OpSll: begin Opout = ir `OpCode; regd = ir `Dest; end
        `OpSlt: begin Opout = ir `OpCode; regd = ir `Dest; end
        `OpSra: begin Opout = ir `OpCode; regd = ir `Dest; end
        `OpXor: begin Opout = ir `OpCode; regd = ir `Dest; end
        `OpGor: begin Opout = ir `OpCode; regd = ir `Dest; end
        `OpLeft: begin Opout = ir `OpCode; regd = ir `Dest; end
        `OpRight: begin Opout = ir `OpCode; regd = ir `Dest; end
        `OpLnot: begin Opout = ir `OpCode; regd = ir `Dest; end
        `OpNeg: begin Opout = ir `OpCode; regd = ir `Dest; end
        `OpLI8: begin Opout = ir `OpCode; regd = ir `Dest; end
        `OpLU8: begin Opout = ir `OpCode; regd = ir `Dest; end
        `OpNOp: begin Opout = `OpNOp; regd = 0; end
        default: begin Opout = ir `Sc; regd = 0; end
    endcase
end
end
endmodule


///////////////////////////////////////////////////////////////////////////////////////////////
module control(halt, reset, clk);
output wire halt;
input reset, clk;
parameter NPROC = 2;
reg  `WORD RegFile `REGSIZE;
reg  `WORD InstMem `MEMSIZE;
reg  `WORD DataMem `MEMSIZE;
reg  `WORD PC, IReg;
wire `WORD NewPC;
wire `Op DecOp;
wire `REGNAME RegDst;
wire [NPROC-1:0] en;
reg  `CSTACKSIZE CallStack,Callstacktemp;
reg  `Op s0Op;
wire `Op s1Op;
wire `WORD s1ScVal,s1altVal;
reg  `REGNAME s0Sc, s0alt, s0Dst, s0RegDst; 
reg `Imm s0Imm;
wire squash;

genvar i;
generate 
  for(i = 0; i < NPROC; i = i + 1) begin:processes
  processor#(NPROC) U(halt, reset, clk, i, s0Imm, s1Op, s1ScVal, s1altVal, s0Op, 
            s0RegDst, s0Sc, s0alt, PC, IReg, enwire);
  wire enwire;
  //wire `REGNAME enleft, enright;
  //wire `REGNAME s1ScVal,s1altVal;
  assign en[i] = processes[i].enwire;
  //assign processes[i].enleft = (i == NPROC-1) ? processes[0].s1ScVal : processes[i+1].s1ScVal;
  //assign processes[i].enright = (i == 0) ? processes[NPROC].s1ScVal : processes[i-1].s1ScVal;
  end
endgenerate
decode MyDecode(DecOp, RegDst, s0Op, IReg);

always @(posedge reset) begin
    PC = 0;
    s0Op = `OpNOp;
    $display("1. Control module.");
    $readmemh1(InstMem);
end

always @(*) IReg = InstMem[PC];

// new pc value. Can come from many places
assign NewPC = ((s1Op == `OpJumpf) && (s1altVal == 0)) ? s1ScVal :
        (s1Op == `OpJump) ? s1ScVal: ((s1Op == `OpCall) &&     
        (en[0] == 1)) ? s1ScVal: ((DecOp == `OpRet) &&(en[0] == 1)) ?          
        CallStack[15:0] : (PC + 1); 

//Squash if necessary
assign squash = (((s1altVal == 0) && (s1Op == `OpJumpf)) || (s0Op == `OpJump) || (s0Op == `OpCall));

//Stage: 1 Inst Fetch
always @(posedge clk)  if (!halt) begin
    IReg <= InstMem[PC];
    if(DecOp == `OpRet && en[0] == 1) CallStack <= {CallStack[63:48], CallStack[63:16]};
    s0Op <= (squash ? `OpNOp : DecOp);
    s0RegDst <= (squash ? 0 : RegDst);
    s0Sc <= IReg`Sc;
    s0alt <= IReg `alt;
    s0Imm <= IReg `Imm;
    PC <= NewPC;
    if(s0Op == `OpCall && en[0] == 1) CallStack <= {CallStack[47:0], PC};
end
endmodule


///////////////////////////////////////////////////////////////////////////////////////////////
module processor#(parameter NPROC = 32)(halt, reset, clk, IPROC, Imm, s1Op, 
      s1ScVal, s1altVal, s0Op, s0RegDst, s0Sc, s0alt, PC, IReg, enwire);
input reset, clk;
input [6:0] IPROC;
input wire `Op s0Op;
input wire `REGNAME s0Sc, s0alt, s0Dst, s0RegDst; 
input wire `Imm Imm;
input wire `WORD PC, IReg;
output reg `Op s1Op;
output reg halt;
output reg `WORD s1ScVal, s1altVal;
output reg [NPROC-1:0] en;
output wire enwire;
//input wire `REGNAME enright, enleft;
reg `WORD ALUResult;
reg  `WORD ScVal, altVal, DstVal;
reg  `REGNAME s1RegDst, s2RegDst;
reg  `WORD s2Val;
reg  `WORD RegFile `REGSIZE;
reg  `WORD DataMem `MEMSIZE;
reg s2halt;
wire [NPROC-1:0] res;
initial assign  en = -1;

//always @(*) #10 $display("clock = %d, reset = %d.", clk, reset);

always @(posedge reset) begin
    $display("%b. Processor module.", IPROC);
    halt = 0;
    s2halt = 0;
    $readmemh0(RegFile);
    RegFile[0] = 0; // zero
    en = -1;
    RegFile[1] = IPROC; // IPROC
    RegFile[2] = NPROC; // NPROC
    s1Op = `OpNOp;
    $readmemh2(DataMem);
end
assign enwire = en[0];
// compute RR ScVal, with value forwarding...
always @(*)
  if ((s0Op == `OpJump) || (s0Op == `OpJumpf) || (s0Op == `OpCall) || (s0Op == `OpLI8) || (s0Op == `OpLU8))      
        ScVal = IReg;
    else ScVal = ((s1RegDst && (s0Sc == s1RegDst)) ? ALUResult :
          ((s2RegDst && (s0Sc == s2RegDst)) ? s2Val :
            RegFile[s0Sc]));

// compute DstVal, with value forwarding
always @(*) DstVal = ((s1RegDst && (s0Dst == s1RegDst)) ? ALUResult :
                      ((s2RegDst && (s0Dst == s2RegDst)) ? s2Val :
                       RegFile[s0Dst]));

// compute altVal, with value forwarding...
always @(*)
    if ((s0Op == `OpJump) || (s0Op == `OpJumpf) || (s0Op == `OpCall) || (s0Op == `OpLI8) || (s0Op == `OpLU8)) 
        altVal = 0;
    else  altVal = (s1RegDst && (s0alt == s1RegDst)) ? ALUResult :
        ((s0alt == s2RegDst)) ? s2Val :RegFile[s0alt];

//Stage: 2 Register Read
always @(posedge clk)  if (!halt) begin
    s1Op <= s0Op;
    s1RegDst <= s0RegDst;
    s1ScVal <= ScVal;
    s1altVal <= altVal;
end

//Stage: 3 ALU and data memory Operations
always @(posedge clk)  if (!halt) begin
    case (s1Op)
      `OpAdd:     ALUResult = s1ScVal + s1altVal;
      `OpAnd:     ALUResult = s1ScVal & s1altVal;
      `OpMul:     ALUResult = s1ScVal * s1altVal;
      `OpOr:      ALUResult = s1ScVal | s1altVal;
      `OpLeft:  begin end
      `OpRight: begin end
      `OpGor:   ALUResult = |en;
      `OpXor:   ALUResult = s1ScVal ^ s1altVal;
      `OpSll:     ALUResult = s1ScVal << (s1altVal & 4'b1111); 
      `OpSra:     ALUResult = s1ScVal >> (s1altVal & 4'b1111);
      `OpLnot:  ALUResult = !s1altVal;
      `OpNeg:     ALUResult = ~s1altVal;
      `OpSlt:   ALUResult = (s1ScVal < s1altVal)? 4'b0001 : 4'b0000;
      `OpLoad:  ALUResult = DataMem[s1ScVal];
      `OpStore: if(en[IPROC]) begin DataMem[s1ScVal] <= s1altVal; end
      `OpLI8:   ALUResult = {{8{Imm[7]}}, Imm};
      `OpLU8:   ALUResult = {Imm, s1altVal[7:0]};
      `OpJump: begin end
      `OpJumpf: begin en[0] <= 0; end
      `OpCall: begin end
      `OpRet: begin end
      `OpNOp: begin end
      `OpAllen:   begin en = {en[NPROC:1], 1'b1}; end
      `OpPOpen: begin en = {en[NPROC], en[NPROC:1]}; end
      `OpPushen: begin en = {en[NPROC:0], en[0]}; end
      default:    begin s2halt <= 1; ALUResult <= s1ScVal; end
    endcase
    s2RegDst <= s1RegDst;
    s2Val <= ALUResult;
end

//Stage: 4  Register Write
always @(posedge clk)  if (!halt) begin
    if (s2RegDst != 0) RegFile[s2RegDst] <= s2Val;
    halt = s2halt;
end
endmodule


///////////////////////////////////////////////////////////////////////////////////////////////
module testbench;
reg reset = 0;
reg clk = 0;
wire halted;
integer i = 0;
control CE(halted, reset, clk);
initial begin
  $dumpfile;
  $dumpvars(0, CE);
  #10 reset = 1;
  #10 reset = 0;
  while (halted == 0) begin
  #10 clk = 1;
  #10 clk = 0;
  i=i+1;
  end
  $finish;
end
endmodule

# EE480-Assigment4
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
`define OpLeft    4'b1001
`define OpRight   4'b1010
`define OpLnot    4'b1011
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
`define OpRet   5'b10101
`define OpNOp   5'b10110
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
        `ScPushen:    Opout = `OpPushen;
        `ScRet:     Opout = `OpRet;
        `ScNOp:     Opout = `OpNOp;
        `ScTrap:    Opout = `OpTrap;
        `ScCall:    Opout = `OpCall;
        `ScJump:    Opout = `OpJump;
        `ScJumpf:   Opout = `OpJumpf;
        endcase
        end
        `OpStore: begin Opout = ir `OpCode; regd <= 0; end
        default: begin Opout = ir `OpCode; regd <= ir `Dest; end
    endcase
end
end
endmodule


module alu(result, OpCode, Inst1, Inst2);
output reg `WORD result;
input wire `Op OpCode;
input wire `WORD Inst1, Inst2;

always @(OpCode, Inst1, Inst2) begin
  case (OpCode)
  `OpAdd:   result = Inst1 + Inst2;
  `OpAnd:   result = Inst1 & Inst2;
  `OpMul:   result = Inst1 * Inst2;
  `OpOr:    result = Inst1 | Inst2;
  `OpGor:   result = Inst2; 
  `OpXor:   result = Inst1 ^ Inst2;
  `OpSll:   result = Inst1 << (Inst2 & 4'b1111); 
  `OpSra:   result = Inst1 >> (Inst2 & 4'b1111);
  `OpLnot:  result = !Inst2;
  `OpNeg:   result = ~Inst2;
  `OpSlt:   result = (Inst1 < Inst2)? 4'b0001 : 4'b0000;
  `OpLeft:  result = Inst2;
  `OpRight: result = Inst2;
  default:  result = Inst1;
  endcase
end
endmodule

module control(halt, reset, clk);
output reg halt;
input reset, clk;
parameter NPROC = 20;
genvar i;
reg  `WORD RegFile `REGSIZE;
reg  `WORD InstMem `MEMSIZE;
reg  `WORD DataMem `MEMSIZE;
reg  `WORD NewPC, IReg, ScVal, altVal, DstVal;
wire `Op DecOp;
wire `REGNAME RegDst;
wire `WORD ALUResult;
reg  `ESTACKSIZE en = 1;
reg  `CSTACKSIZE CallStack,Callstacktemp;
reg  `Op s0Op, s1Op, s2Op;
reg  `REGNAME s0Sc, s0alt, s0Dst, s0RegDst, s1RegDst, s2RegDst;
reg  `WORD PC;
reg  `WORD s1ScVal, s1altVal;
reg  `WORD s2Val;


generate 
	for(i = 1; i <= NPROC; i = i + 1) begin:processes
	processor U(i,ALUResult, s1Op, s1ScVal, s1altVal);
	end
endgenerate

always @(reset) begin
    halt = 0;
    PC = 0;
    RegFile[0] = 0; // zero
    RegFile[1] = 0; // IPROC
    RegFile[2] = 1; // NPROC
    en = -1; // all 1s
    s0Op = `OpNOp;
    s1Op = `OpNOp;
    s2Op = `OpNOp;
    $readmemh0(RegFile);
    $readmemh1(InstMem);
    $readmemh2(DataMem);
end


always @(*) IReg = InstMem[PC];

// new pc value. Can come from many places
always @(*) begin 
    NewPC = ((s1Op == `OpJumpf) && (s1altVal == 0)) ? s1ScVal :
        (s1Op == `OpJump) ? s1ScVal: ((s1Op == `OpCall) &&     
        (en[0] == 1)) ? s1ScVal: ((s1Op == `OpRet) &&(en[0] == 1)) ?          
        CallStack[15:0] : (PC + 1); 
    Callstacktemp = CallStack;
    if(s1Op == `OpJumpf && s1altVal == 0) begin
        en[0] = ~en[0];
    end
    if(s1Op == `OpCall && en[0] == 1) begin
    
        CallStack = {Callstacktemp[47:0], PC};
    end
    if(s1Op == `OpRet && en[0] == 1) begin
        CallStack = {Callstacktemp[63:48], Callstacktemp[63:16]};
    end
end

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



// Inst Fetch
always @(posedge clk) if (!halt) begin
    s0Op <= (DecOp == `OpTrap) ? `OpNOp : DecOp;
    s0RegDst <= (DecOp == `OpTrap) ? 0 : RegDst;
    s0Sc <= IReg`Sc;
    s0alt <=IReg `alt;
    PC <= NewPC;
end

// Register Read
always @(posedge clk) if (!halt) begin
    s1Op <= s0Op;
    s1RegDst <= s0RegDst;
    s1ScVal <= ScVal;
    s1altVal <= altVal;
end

// ALU and data memory Operations
always @(posedge clk) if (!halt) begin
    s2Op <= s1Op;
    s2RegDst <= s1RegDst;
    s2Val <= ((s1Op == `OpLoad) ? DataMem[s1ScVal] : ALUResult);
    if (s1Op == `OpStore) DataMem[s1ScVal] <= s1altVal;
end

// Register Write
always @(posedge clk) if (!halt) begin
    if (s2RegDst != 0) RegFile[s2RegDst] <= s2Val;
end
endmodule

module processor(t,result,OpCode, Inst1, Inst2);
output wire `WORD result;
input [6:0] t;
input wire `Op OpCode;
input wire `WORD Inst1, Inst2;
generate
	alu ALU(result, OpCode, Inst1, Inst2);
endgenerate




initial
	 $display("Processing Element %d!\n", t);
endmodule

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
  while (!halted && (i < 50)) begin
  #10 clk = 1;
  #10 clk = 0;
  i=i+1;
  end
  $finish;
end
endmodule

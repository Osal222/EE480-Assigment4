add   $.d,$.s,$.t                       := 0:4   .d:4  .s:4  .t:4
and   $.d,$.s,$.t                       := 1:4   .d:4  .s:4  .t:4
mul   $.d,$.s,$.t                       := 2:4   .d:4  .s:4  .t:4
or    $.d,$.s,$.t                       := 3:4   .d:4  .s:4  .t:4
sll   $.d,$.s,$.t                       := 4:4   .d:4  .s:4  .t:4
slt   $.d,$.s,$.t                       := 5:4   .d:4  .s:4  .t:4
sra   $.d,$.s,$.t                       := 6:4   .d:4  .s:4  .t:4
xor   $.d,$.s,$.t                       := 7:4   .d:4  .s:4  .t:4
gor   $.d,$.s                           := 8:4   0:4   .d:4  .s:4 
left  $.d,$.s                           := 9:4   0:4   .d:4  .s:4 
right $.d,$.s                           := 10:4  0:4   .d:4  .s:4 
lnot  $.d,$.s                           := 11:4  0:4   .d:4  .s:4 
neg   $.d,$.s                           := 12:4  0:4   .d:4  .s:4 
li8   $.d,.i8                           := 13:4  .d:4  .i8:8
lu8   $.d,.i8                           := 14:4  .d:4  .i8:8
load  $.d,$.s                           := 15:4  0:4   .d:4  .s:4 
store $.d,$.s                           := 15:4  1:4   .d:4  .s:4
allen                                   := 15:4  2:4   0:4   0:4 
popen                                   := 15:4  3:4   0:4   0:4 
pushen                                  := 15:4  4:4   0:4   0:4 
ret                                     := 15:4  5:4   0:4   0:4 
nop                                     := 15:4  6:4   0:4   0:4 
trap                                    := 15:4  7:4   0:4   0:4 
call .addr                              := 15:4  8:4   0:4   0:4   .addr:16 
jump .addr                              := 15:4  9:4   0:4   0:4   .addr:16
jumpf $.d,.addr                         := 15:4  10:4  0:4   .d:4  .addr:16
li  $.d,.i16 ?((.i16>-129)&&(.i16<128)) :={13:4  .d:4  .i8:8}
li  $.d,.i16                            :={13:4  .d:4  .i8:8
                                           14:4  .d:4  (.i16>>8):8}
.const{zero IPROC NPROC sp fp rv u0 u1 u2 u3 u4 u5 u6 u7 u8 u9}

.segment .text 16 0x10000 0 .VMEM
.segment .data 16 0x10000 0 .VMEM
.const 0 .lowfirst                



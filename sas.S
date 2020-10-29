
##############################
# SAS assembly codes
# All SAS assembly codes are 1 byte in length.
.section .data
.macro CODE code , namelen, name
  .equ \name , \code
  .byte \code
  .byte \namelen
  .ascii "\name"
.endm

sas_alulist:
CODE 0x00, 3, "ext"      # extension call, see extlist
CODE 0x01, 4, "vnxt"     # indirect jump to vip, inc vip by cell
CODE 0x02, 4, "zjmp"     # pop p1 p0, jump to p0 if p1=0
CODE 0x03, 3, "jmp"      # pop p0 and jump to it
CODE 0x04, 4, "call"     # rpush of eip, jmp to p0
CODE 0x05, 3, "ret"      # rpop and jmp to it
CODE 0x06, 3, "shr"      # shift p1 right by p0 `( p1 p0 -- p1>>p0 )`
CODE 0x07, 3, "shl"      # shift p1 left by p0 `( p1 p0 -- p1<<p0 )`
CODE 0x08, 3, "and"      # bitwise AND of top 2 stack values
CODE 0x09, 2, "or"       # bitwise OR of top 2 stack values
CODE 0x0A, 3, "xor"      # bitwise XOR of top 2 stack values
CODE 0x0B, 3, "add"      # add top two stack values
CODE 0x0C, 3, "inv"      # bitwise invert top stack value
CODE 0x0D, 3, "neg"      # negate top stack value (2's compliment)
CODE 0x0E, 3, "eqz"      # compare top stack value to 0, push true if equal
CODE 0x0F, 2, "eq"       # compare top two stack values for equality
CODE 0x10, 2, "lt"       # push true if p1 < p0, else false
CODE 0x11, 3, "lte"      # push true if p1 <= p0, else false
#CODE0x12                # reserved for gt, use `lte eq0`
#CODE0x13                # reserved for gte, use `lt eq0`
CODE 0x14, 4, "swap"     # swap stack values
CODE 0x15, 3, "dup"      # duplicate stack value `( p0 -- p0 p0 )`
CODE 0x16, 4, "drop"     # drop stack value `( p0 -- )`
CODE 0x17, 4, "rpsh"     # push value from dstack -> rstack
CODE 0x18, 4, "rpop"     # pop value from rstack -> dstack
CODE 0x19, 4, "xget"     # fetch "code-size" address onto the stack
CODE 0x1A, 4, "xset"     # setre p1 at "code-size" p0 address
CODE 0x1B, 3, "get"      # load 32bit address onto the stack (consumes 1 or 2 values)
CODE 0x1C, 3, "set"      # store value at 32bit address (consumes 2 or 3 values)
CODE 0x1D, 4, "getb"     # load byte at 32bit address onto the stack
CODE 0x1E, 4, "setb"     # store byte at 32bit address

sas_extlist: # used with ext
CODE 0x00, 6, "getdsp"   # push dsp to stack
CODE 0x01, 6, "setdsp"   # set dsp to p1
CODE 0x01, 6, "getrsp"   # push rsp to stack
CODE 0x00, 6, "setrsp"   # set rsp to p1
CODE 0x00, 6, "getvip"   # push vip to stack
CODE 0x00, 6, "setvip"   # set vip to p1
CODE 0x00, 6, "geteip"   # push eip to stack (for relative jump)
CODE 0x00, 7, "setdmin"  # set `dstack_min` to p1
CODE 0x00, 7, "setdmax"  # set `dstack_max` to p1
CODE 0x00, 7, "setrmin"  # set `rstack_min` to p1
CODE 0x00, 7, "setrmax"  # set `rstack_max` to p1
# ...0x0F reserved

.macro SASDONE
  jmp si
.endm

.equ LINUX_SYSCALL,0x80  # linux kernel interrupt vector
.equ SYS_EXIT, 1
sysexit: # ebx:rc ( -- ) \ exit the system with return code
  movl $SYS_EXIT, %eax
  int $LINUX_SYSCALL

##############################
# SAS x86 interpreter and runtime
# 
# REGISTERS: The below is a helpful register reference for x86
# general 32bit:    %eax   %ebx   %ecx   %edx  %edi  %esi
# from above 16bit:  ax     bx     cx     dx   none  none
# from above 8bit: al ah  bl bh  cl ch  dl dh  none  none
# special:  %esp: stack ptr, our return stack
#           %ebp: base ptr, our data stack
#           %eip: cpu inst ptr, not OUR instruction pointer
#           %edi: our sas-instr ptr
#           %esi: our forth xt-inst ptr
#           %eflags: we won't deal with this

.macro SASRUN label
si_\label :
  jmp \label
  .align 4
.endm

.section .text
si: # sas interpret
  movl $si_sas_zjmp, %ebx
  subl $si_sas_vnxt, %ebx
  call sysexit
  xor %eax, %eax    # zero eax
  movb (%edi), %al
  testb 0x80, %al
  jnz sas_literal
  shll $2, %eax     # multiply eax by 4, codesize of jmp
  addl $si_jt, %eax # add jt address to it
  pushl %eax # push location onto "return" stack
  ret # and jump/return to it. It will call `jmp si`
.align 4
si_jt: # jump table
  SASRUN sas_ext
  SASRUN sas_vnxt
  SASRUN sas_zjmp

sas_literal:
  SASDONE

sas_ext:
  SASDONE

sas_vnxt:
  SASDONE

sas_zjmp:
  SASDONE



.globl _start
_start:
  call si
  movl $0, %ebx
  call sysexit

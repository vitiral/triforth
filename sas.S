
.section .data
##############################
# SAS assembly codes
# All SAS assembly codes are 1 byte in length.
.macro CODE code , namelen, name
  .equ \name , \code
  .byte \code
  .byte \namelen
  .ascii "\name"
.endm

.equ lit, 0x80
.equ rlit, 0x40

sas_alulist:
CODE 0x00, 4, getr     # get register value, see reglist
CODE 0x01, 4, setr     # set register value, see reglist
CODE 0x02, 4, vnxt     # indirect jump to vip, inc vip by cell
CODE 0x03, 4, zjmp     # pop p1 p0, jump to p0 if p1=0
CODE 0x04, 3, jmp      # pop p0 and jump to it
CODE 0x05, 4, call     # push edi+1 onto rstack and jmp to p0
#CODE0x06
CODE 0x07, 3, shr      # shift p1 right by p0 `( p1 p0 -- p1>>p0 )`
CODE 0x08, 3, shl      # shift p1 left by p0 `( p1 p0 -- p1<<p0 )`
CODE 0x09, 3, and      # bitwise AND of top 2 stack values
CODE 0x0A, 2, or       # bitwise OR of top 2 stack values
CODE 0x0B, 3, xor      # bitwise XOR of top 2 stack values
CODE 0x0C, 3, add      # add top two stack values
CODE 0x0D, 3, inv      # bitwise invert top stack value
CODE 0x0E, 3, neg      # negate top stack value (2's compliment)
CODE 0x0F, 3, eqz      # compare top stack value to 0, push true if equal
CODE 0x10, 2, eq       # compare top two stack values for equality
CODE 0x11, 2, lt       # push true if p1 < p0, else false
CODE 0x12, 3, lte      # push true if p1 <= p0, else false
#CODE0x13                # reserved for gt, use `lte eq0`
#CODE0x14                # reserved for gte, use `lt eq0`
CODE 0x15, 4, swap      # swap stack values
CODE 0x16, 3, dup       # duplicate stack value `( p0 -- p0 p0 )`
CODE 0x17, 4, drop      # drop stack value `( p0 -- )`
CODE 0x18, 4, rpsh      # push value from dstack -> rstack
CODE 0x19, 4, rpop      # pop value from rstack -> dstack
CODE 0x1A, 4, xget      # fetch "code-size" address onto the stack
CODE 0x1B, 4, xset      # setre p1 at "code-size" p0 address
CODE 0x1C, 3, get       # load 32bit address onto the stack (consumes 1 or 2 values)
CODE 0x1D, 3, set       # store value at 32bit address (consumes 2 or 3 values)
CODE 0x1E, 4, getb      # load byte at 32bit address onto the stack
CODE 0x1F, 4, setb      # store byte at 32bit address

sas_reglist: # setr and getr
CODE 0x00, 3, dsp
CODE 0x01, 3, rsp
CODE 0x02, 3, vip
CODE 0x03, 3, sip
CODE 0x04, 4, dmin   # set `dstack_min` to p1
CODE 0x05, 4, dmax   # set `dstack_max` to p1
CODE 0x06, 4, rmin   # set `rstack_min` to p1
CODE 0x07, 4, rmax   # set `rstack_max` to p1
# ...0x0F reserved
CODE 0x10, 7, sasexit   # exit the SAS interpreter, used for tests written in assembly
CODE 0x11, 7, ReaxSYS   # set eax and execute a linux syscall (int $0x80)
CODE 0x12, 7, Rebx
CODE 0x13, 7, Recx
CODE 0x14, 7, Redx

##############################
# SAS x86 interpreter and runtime
#
# REGISTERS: The below is a helpful register reference for x86
# general 32bit:    %eax   %ebx   %ecx   %edx  %edi  %esi
# from above 16bit:  ax     bx     cx     dx   none  none
# from above 8bit: al ah  bl bh  cl ch  dl dh  none  none
# special:  %esp: stack ptr, our return stack pointer (rsp)
#           %ebp: base ptr, our data stack pointer (dsp)
#           %eip: cpu inst ptr, not OUR instruction pointer
#           %edi: our sas-instr ptr (sip)
#           %esi: our forth xt-inst ptr (vip)
#           %eflags: we won't deal with this
snap_eax: .int 0
snap_ebx: .int 0
snap_ecx: .int 0
snap_edx: .int 0

snapshotRegisters:
  movl %eax, snap_eax
  movl %ebx, snap_ebx
  movl %ecx, snap_ecx
  movl %edx, snap_edx
  ret

restoreRegisters:
  movl snap_eax, %eax
  movl snap_ebx, %ebx
  movl snap_ecx, %ecx
  movl snap_edx, %edx
  ret

.equ RC_DBG,42
.equ RC_TODO,99
.equ RC_UNREACHABLE,101
.equ RC_SASBAD,102
.equ RC_REGBAD,103
.equ RC_EDIBAD,104
.equ RC_IOERR,105
.equ RC_HEXBAD,106

.equ LINUX_SYSCALL,0x80  # linux kernel interrupt vector
.equ SYS_EXIT, 1
.equ SYS_READ, 3
.equ SYS_WRITE, 4

.macro dpushi v
  subl $2, %ebp
  movw \v , (%ebp)
.endm

.macro dpushd v
  subl $4, %ebp
  movl \v , (%ebp)
.endm

.macro dpopi v
  movw (%ebp), \v
  addl $2, %ebp
.endm

.macro rpushi v
  subl $2, %esp
  movw \v , (%esp)
.endm

.macro rpopi v
  movw (%esp), \v
  addl $2, %esp
.endm

.macro sysexit v
  movl \v , %ebx
  call _sysexit
.endm

_sysexit: # ebx:rc ( -- ) \ exit the system with return code
  movl $SYS_EXIT, %eax
  int $LINUX_SYSCALL

.macro UNREACHABLE
  call _unreachable
.endm

_unreachable:
  sysexit $RC_UNREACHABLE

.macro dbgexit
  sysexit $RC_DBG
.endm

.macro TODO
  call todo
.endm

todo:
  sysexit $RC_TODO

sysemit: # ( b -- ) emits a single byte to emitFd
  # We define this here to aid in debugging
  dpopi %ax
  rpushi %ax # put byte in return stack
  xor %ebx, %ebx
  movw emitFd, %bx   # fd = r0
  movl %esp, %ecx # &buf=&r0
  movl $1, %edx       # bytesToWrite
  movl $SYS_WRITE, %eax
  int $LINUX_SYSCALL
  cmpl $0, %eax
  jl 1f
  rpopi %ax # drop b
  ret
1:
  sysexit $RC_IOERR

bl_toascii: # ( b -- ascii ) \ convert number to ascii equivalent
  cmpb $9, %bl
  jg 1f
  # number <= 9
  addb $'0', %bl
  jmp 2f
1: # number > 9
  subb $10, %bl
  cmpb $23, %bl
  jg 3f
  addb $'A', %bl
2: # return
  ret
3: # error
  sysexit $RC_HEXBAD

.macro PNTBX v  # used for debugging
  dpushi \v
  call pntbx
.endm


_pntbx:
  xor %ebx, %ebx
  dpopi %bx
  dpushi %bx    # cache on stack
  shrb $4, %bl # bl=top 4 bits
  call bl_toascii
  dpushi %bx
  call sysemit
  xor %ebx, %ebx
  dpopi %bx
  andb $0x0F, %bl # bl=bot 4 bits
  call bl_toascii
  dpushi %bx
  call sysemit
  ret

_pntix: # ( i -- ) \ print i as hex
  dpopi %ax
  movw %ax, %bx
  andw $0xFF, %bx
  dpushi %bx
  shr $8, %ax
  dpushi %ax
  call _pntbx
  call _pntbx
  ret

_pntdx: # ( d -- ) \ print as double hex
  dpopi %ax
  dpopi %bx
  dpushi %ax
  dpushi %bx
  call _pntix
  dpushi $'_'
  call sysemit
  call _pntix
  ret

pntbx: # ( b -- ) \ print a byte as hex followed by space
  call snapshotRegisters
  call _pntbx
  dpushi $' '
  call sysemit
  call restoreRegisters
  ret

pntix: # ( d -- ) \ print as i hex
  call snapshotRegisters
  call _pntix
  dpushi $' '
  call sysemit
  call restoreRegisters
  ret

pntdx: # ( d -- ) \ print as d hex
  call snapshotRegisters
  dpushi $'['
  call sysemit
  call _pntdx
  dpushi $']'
  call sysemit
  dpushi $' '
  call sysemit
  call restoreRegisters
  ret

pntcode: # ( -- ) \ print a byte in form "[XX] "
  call snapshotRegisters
  dpushi %ax
  dpushi $'['
  call sysemit
  call _pntbx
  dpushi $']'
  call sysemit
  dpushi $' '
  call sysemit
  call restoreRegisters
  ret

pntstack:
  call snapshotRegisters
  dpushi $'S'
  call sysemit
  dpushi $':'
  call sysemit
  movl $dstackMax, %eax
  subl $2, %eax
1:
  cmp %ebp, %eax
  jl 2f
  movw (%eax), %cx
  dpushi %cx
  call pntix
  subl $2, %eax
  jmp 1b
2:
  dpushi $'\n'
  call sysemit
  call restoreRegisters
  ret

.macro SASDONE
  inc %edi
  jmp si
.endm

.macro sas_bad10 # do sas_bad 0m16 times
  .int sas_bad, sas_bad, sas_bad, sas_bad, sas_bad, sas_bad, sas_bad, sas_bad
  .int sas_bad, sas_bad, sas_bad, sas_bad, sas_bad, sas_bad, sas_bad, sas_bad
.endm

.macro todo10 # do todo 0m16 times
  .int todo, todo, todo, todo, todo, todo, todo, todo
  .int todo, todo, todo, todo, todo, todo, todo, todo
.endm

.macro sas_rlit10 # do sas_rlit 0m16 times
  .int sas_rlit, sas_rlit, sas_rlit, sas_rlit, sas_rlit, sas_rlit, sas_rlit, sas_rlit
  .int sas_rlit, sas_rlit, sas_rlit, sas_rlit, sas_rlit, sas_rlit, sas_rlit, sas_rlit
.endm

.macro sas_lit10 # do sas_lit 0m16 times
  .int sas_lit, sas_lit, sas_lit, sas_lit, sas_lit, sas_lit, sas_lit, sas_lit
  .int sas_lit, sas_lit, sas_lit, sas_lit, sas_lit, sas_lit, sas_lit, sas_lit
.endm


si: # sas interpret
  xor %eax, %eax    # zero eax
  movb (%edi), %al
  call pntcode
  jmp *si_jt(,%eax,4)
.align 4
si_jt: # jump table
  .int sas_getreg
  .int sas_setr
  .int sas_vnxt
  .int sas_zjmp
  .int sas_jmp
  .int sas_call
  .int sas_bad # reserved
  .int todo # sas_shr
  .int todo # sas_shl
  .int todo # sas_and
  .int todo # sas_or
  .int todo # sas_xor
  .int todo # sas_add
  .int todo # sas_inv
  .int todo # sas_neg
  .int todo # sas_eqz
  .int todo # sas_eq
  .int todo # sas_lt
  .int todo # sas_lte
  .int sas_bad # reserved
  .int sas_bad # reserved
  .int sas_swap
  .int sas_dup
  .int sas_drop # sas_drop
  .int sas_rpsh
  .int sas_rpop
  .int todo # sas_xget
  .int todo # sas_xset
  .int todo # sas_get
  .int todo # sas_set
  .int todo # sas_getb
  .int todo # 0x1F: sas_setb
  sas_bad10           # ...0x2F : reserved
  sas_bad10        # ...0x3F : reserved
  sas_rlit10       # ...0x4F : rlit10
  sas_rlit10       # ...0x5F : rlit10
  sas_rlit10       # ...0x6F : rlit10
  sas_rlit10       # ...0x7F : rlit10
  sas_lit10        # ...0x8F : lit10
  sas_lit10        # ...0x9F : lit10
  sas_lit10        # ...0xAF : lit10
  sas_lit10        # ...0xBF : lit10
  sas_lit10        # ...0xCF : lit10
  sas_lit10        # ...0xDF : lit10
  sas_lit10        # ...0xEF : lit10
  sas_lit10        # ...0xFF : lit10

sas_lit:
  xor %eax, %eax
  movb (%edi), %al
  andb $0x7F, %al
  dpushi %ax
  SASDONE

sas_rlit:
  xor %eax, %eax
  movb (%edi), %al
  andb $0x3F, %al
  leal (%edi,%eax,2), %eax
  andl $~0x01, %eax   # make eax "aligned" by dropping lowest bit
  movw (%eax), %ax
  dpushi %ax
  SASDONE

sas_getreg:
  xor %eax, %eax
  movb (%edi), %al
  cmpb $0x04, %al
  jge reg_bad
  jmp *sas_getreg_jt(,%eax,4)
sas_getreg_jt:
  .int sg_dsp
  .int sg_rsp
  .int sg_vip
  .int sg_sip
sg_dsp:
  movl %ebp, %eax
  jmp sg_convert
sg_rsp:
  movl %esp, %eax
  jmp sg_convert
sg_vip:
  movl %esi, %eax
  jmp sg_convert
sg_sip:
  movl %edi, %eax
  jmp sg_convert
sg_convert:
  subl xstart, %eax
  dpushi %ax
  SASDONE

sas_setr:
  dpushd $0x88010000  # FIXME: remove
  call pntdx
  call pntstack
  xor %eax, %eax
  dpopi %ax
  cmpw $0x15, %ax
  jge reg_bad
  jmp *sas_setr_jt(,%eax,4)
sas_setr_jt:
  .int ss_dsp
  .int ss_rsp
  .int ss_vip
  .int ss_eip
  .int reg_bad # 0x04 TODO: finish these
  .int reg_bad
  .int reg_bad
  .int reg_bad
  .int reg_bad # 0x08 reserved
  .int reg_bad
  .int reg_bad
  .int reg_bad
  .int reg_bad
  .int reg_bad
  .int reg_bad
  .int reg_bad # 0x0F reserved
  .int ss_sasexit
  .int ss_eaxSYS
  .int ss_ebx
  .int ss_ebx
  .int ss_ecx
  .int ss_edx
ss_dsp: TODO
ss_rsp: TODO
ss_vip: TODO
ss_eip: TODO
ss_sasexit: ret
ss_eaxSYS:
  dpopi %ax # note: eax already zeroed
  int $LINUX_SYSCALL
  dpushi %ax
  SASDONE
ss_ebx:
  xor %ebx, %ebx
  dpopi %bx
  SASDONE
ss_ecx:
  xor %ecx, %ecx
  dpopi %cx
  SASDONE
ss_edx:
  xor %edx, %edx
  dpopi %dx
  SASDONE

sas_vnxt:
  TODO

sas_zjmp:
  dpopi %ax
  cmpw $0, %ax
  je sas_jmp
  SASDONE

sas_jmp:
  dpushd $0x77770003  # FIXME: remove
  call pntdx
  call pntstack
  xor %eax, %eax
  dpopi %ax
  addl $xstart, %eax
  dpushd %eax  # FIXME: remove
  call pntdx
  movl %eax, %edi
  jmp si

sas_call: # push edi+1 onto rstack and call p0
  dpushd $0x77770004
  call pntdx
  call pntstack
  inc %edi
  movl %edi, %eax
  subl $xstart, %eax
  cmpl $0xFFFF, %eax
  jg edi_bad
  rpushi %ax
  dpushi %ax
  call pntix
  jmp sas_jmp

sas_swap:
  dpopi %ax
  dpopi %bx
  dpushi %ax
  dpushi %bx
  SASDONE

sas_dup:
  dpopi %ax
  dpushi %ax
  dpushi %ax
  SASDONE

sas_drop:
  dpopi %ax
  SASDONE

sas_rpsh:
  dpopi %ax
  rpushi %ax
  SASDONE

sas_rpop:
  dpushd $0x11110003  # FIXME: remove
  call pntdx
  rpopi %ax
  dpushi %ax
  dpushi %ax # FIXME: remove
  call pntix
  SASDONE

reg_bad: sysexit $RC_REGBAD
sas_bad: sysexit $RC_SASBAD
edi_bad: sysexit $RC_EDIBAD

############################################################
# Forth Core Code
.align 0x100
xstart: # code (x) address offset, used by xget, xset, jmp, etc
.equ RSTACK_SIZE, 1024
rstackMin: .space RSTACK_SIZE # return stack
rstackMax:
.equ DSTACK_SIZE, 256
dstackMin: .space DSTACK_SIZE # data stack
dstackMax:


############################################################
# Forth Words

.macro RLIT constaddr
77:
  # Get the (16bit) cell difference from the  part of current address (with bit
  # 1 dropped) to constaddr
  # 2 3 4 5 6 7 8  \ cell distance from 2 or 3 to 8 is 3.
  # ^ ^         ^  \ ((8-2) + 1) / 2 == 7 / 2 == 3 OK
  # from       to  \ ((8-3) + 1) / 2 == 6 / 2 == 3 OK
  .byte rlit + ((( \constaddr - 77b ) + 1 )/2)
.endm


emit: # ( b -- ) \ emit a single byte to emitFd
  .byte rpsh, lit+1, lit+rsp, getr
  RLIT emitFd
  RLIT emit_xsc3 # ( bytesToWrite=1 &buf=rsp fd xsyscall3)
  .byte call, rpop, drop, drop, rpop, jmp # TODO: do error checking instead of drop
.align 2
  emitFd: .word 2 # STDERR
  emit_xsc3: .word syscall3 - xstart

syscall0: # ( sc -- ax )
  .byte lit+ReaxSYS, setr, rpop, jmp

syscall1: # ( p0 sc -- ax )
  .byte rpsh, lit+Rebx, setr
  .byte rpop, lit+ReaxSYS, setr
  .byte rpop, jmp

syscall2: # ( p1 p0 sc -- ax )
  .byte rpsh, lit+Rebx, setr, lit+Recx, setr
  .byte rpop, lit+ReaxSYS, setr
  .byte rpop, jmp

syscall3: # ( p2 p1 p0 sc -- ax )
  .byte rpsh, lit+Rebx, setr, lit+Recx, setr, lit+Redx, setr
  .byte rpop, lit+ReaxSYS, setr
  .byte rpop, jmp

# emit: # ( c -- )
#   # emits a single byte to emitFd
#   dpop %eax
#   movb %al, emitBuffer
#   movl emitFd, %ebx   # fd
#   movl $emitBuffer, %ecx # &buf
#   movl $1, %edx       # bytesToWrite
# syswrite: # %ebx:fd %ecx:&buff %edx:bytesToWrite ( -- )
#   # Direct call to SYS_WRITE syscall with error checking.
#   movl $SYS_WRITE, %eax
#   int $LINUX_SYSCALL
#   cmpl $0, %eax
#   jl 1f
#   ret
# 1:
#   movl %eax, panicErr
#   call *(panic)
#   jmp unreachable

############################################################
# Tests


t0:
  movl $t0_d, %edi
  call si
t0_d:
  .byte lit+41, lit+SYS_EXIT
  RLIT t0_xsys1
  .byte call
  # RLIT t0_xemit
  # .byte dup, lit+'4', swap, call, dup, lit+'2', swap, call, lit+'\n', swap, call
  # .byte lit+41, lit+Rebx, setr, lit+1, lit+ReaxSYS, setr # simplest test: exit with rc=41
.align 2
t0_xemit: .word emit-xstart
t0_xsys1: .word syscall1-xstart

.equ andit, (0x80 & 0x84)
.equ orit, (0x80 | 0x04)
.globl _start
_start:
  movl $rstackMax, %esp  # clear rstack
  movl $dstackMax, %ebp  # clear dstack
  dpushd $xstart
  call pntdx
  dpushd $t0_xemit
  call pntdx
  dpushd $emit
  call pntdx
  dpushd $(emit-xstart)
  call pntdx
  dpushd $0x12345678
  call pntdx
  dpushi $'\n'
  call sysemit
  call t0
  sysexit $0

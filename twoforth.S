// general 32bit:    %eax   %ebx   %ecx   %edx  %edi  %esi
// from above 16bit:  ax     bx     cx     dx   none  none
// from above 8bit: al ah  bl bh  cl ch  dl dh  none  none
// special:  %esp: stack ptr, our return stack ptr (RSP)
//           %ebp: base ptr, our parameter stack ptr (PSP)
//           %eip: cpu inst ptr
//           %esi: our forth xt-inst ptr
//           %eflags: not used directly here
//
// basic types:
// o ot (One byte)
// w wot (tWO bytes)
// f fot (FOur bytes)
// g got (eiGht bytes)

//#############################
// Constants
// It's better to define these at the top of the file. Use these as a reference
// as you encounter them.
.equ FALSE, 0
.equ TRUE, 1
.equ CELL, 4

.equ LINUX_SYSCALL,0x80  # linux kernel interrupt vector
.equ SYS_OPEN, 5
.equ SYS_WRITE, 4
.equ SYS_READ, 3
.equ SYS_CLOSE, 6
.equ SYS_EXIT, 1

// File open options
.equ O_RDONLY, 0x0000  # open for reading only
.equ O_WRONLY, 0x0001  # open for writing only
.equ O_RDWR,   0x0002  # open for reading and writing

// Standard file descriptors
.equ STDIN, 0
.equ STDOUT, 1
.equ STDERR, 2

// Our Return codes
.equ RC_DBG, 99 # 99 problems and debugging ain't one, `call dbgexit`
.equ RC_TESTFAIL, 100
.equ RC_UNREACHABLE, 101
.equ RC_INVALIDRESET, 102
.equ RC_ARRAYBOUNDS, 103
.equ RC_PANIC, 104
.equ RC_STACK_UNDERFLOW, 105
.equ RC_STACK_OVERFLOW, 106
.equ RC_STACK_OVERFLOW, 106
.equ RC_ASM_INVARIANT, 107

################################################################################
#### DATA SECTION -- mutable memory must go here.                           ####
################################################################################
//###############
// Data layout
// |-rstack----|-pstack-|-"code"-------------------|-memory---...
//
// Code contains _only_ code (not the word dictionary). The word dictionary is
// a normal linked-list, except the name, flags and type is all kept inside of
// "memory" and so can be dropped once the "compilation" is complete.
.section .data
.align 4

######
# Stacks
.equ RSTACK_SIZE, 0x2000
rstackExtraRoot: .space 0x4
rstackMinRoot: .space RSTACK_SIZE # return stack
rstackMaxRoot:
.equ PSTACK_SIZE, 0x100
pstackExtraRoot: .space 0x20      # guaranteed space above the pstack
pstackMinRoot: .space PSTACK_SIZE # data stack
pstackMaxRoot:
rstackMin: .int 0
rstackMax: .int 0
pstackMin: .int 0
pstackMax: .int 0

######
# Global Variables / State / Buffers
compileState: .int 0 # 0=runmode else=compiling
initialEsp: .int 0 # initial %esp register, use to get args/ENV
pntFd: .int STDERR # where to PNT to
compilingType: .int 0xFFFFFFFF
sourceFd: .int STDIN   # where the source code is read from
.equ SOURCE_STR_CNT_MAX, 0x46  # maximim bytes of source read
.equ WORD_CNT_MAX, 0x23        # maximum word size
.equ WORD_STACK_SIZE, WORD_CNT_MAX+1
sourceEof: .byte 0 # Whether an EOF was encountered at the last read
sourceCStr: .byte 0 # Used for reading source word
sourceStr: .space SOURCE_STR_CNT_MAX
wordCStr: .byte 0
wordStr: .space WORD_CNT_MAX
.align 4
errno: .int 0
panic: .int panicDefault # function to call when panicing
panicErr: .int 0    # error to print while panicing
panicNtStr: .int 0  # error message to print while panicing

# Offsets into the global "dict struct"
.equ GLOBAL_DICT_OFFSET_STRUCT,    0x0  # start of the struct
.equ GLOBAL_DICT_OFFSET_CODE_MIN,  0x0
.equ GLOBAL_DICT_OFFSET_CODE_HERE, 0x4
.equ GLOBAL_DICT_OFFSET_CODE_MAX,  0x8
.equ GLOBAL_DICT_OFFSET_META_MIN,  0xC
.equ GLOBAL_DICT_OFFSET_META_HERE, 0x10
.equ GLOBAL_DICT_OFFSET_META_MAX,  0x14
.equ DEFAULT_DICT_BUCKETS_CNT, 0x1000

.macro DICT_GLOBAL_GET_ADDR field, dictGlobalAddr
  // Get a field from the dictionary (STRUCT, [CODE|META]_[MIN|MAX|HERE])
  // dictGlobalAddr becomes the address of the field
  addl $GLOBAL_DICT_OFFSET_\field, \dictGlobalAddr
.endm

.macro DECLARE_DICT type
  dict\type :
  dictCodeMin\type  : .int 0
  dictCodeHere\type : .int 0
  dictCodeMax\type  : .int 0
  dictMetaMin\type  : .int 0
  dictMetaHere\type : .int 0
  dictMetaMax\type  : .int 0
.endm
DECLARE_DICT RTime
DECLARE_DICT CTime

dictBucketsAddr : .int 0
dictBucketsAndValue : .int 0 # Value used to MOD via AND (power_of_2-1)

# LocalTime dict, mostly implemented in forth
dictLTimeRoot: .int 0
dictLTimeMin:  .int 0
dictLTimeHere: .int 0
dictLTimeMax:  .int 0

######
# Debugging / Testing Variables
registerSnapshot: .space 0x20
testingNtStr: .int 0
panicRestore: .int 0
testSpace_00: .int 0
testSpace_04: .int 0
testSpace_08: .int 0
testSpace_0C: .int 0
testSpace_10: .int 0

################################################################################
#### PURE ASSEMBLY                                                          ####
################################################################################
// The following are pure assembly functions whose sole purpose is to make
// developing in assembly easier. None of these will be linked directly to
// forth words (although some are used in their implementations).
//
// More importantly, these make it much easier to debug both assembly and forth
// as you are writing the bootstrapper. Feel free to implement them yourself and
// then look at how I have done it. The basic functionality we need is to be
// able to print registers and the stack, along with an arbitrary number. This
// will allow us to more easily reason about code that is being executed.
//
// Finally, we develop a way to "panic", printing out debug information in the
// process. We use indirection so that we can expect panics in our tests.
.align 4
.section .text
.macro ZERO reg
  // Zero a register. Uses xor which is more compact than mov
  xor \reg, \reg
.endm

.macro SET_AB a, b
  movl \a, %eax
  movl \b, %ebx
.endm
.macro SET_ABC a, b, c
  SET_AB \a, \b
  movl \c, %ecx
.endm
.macro SET_ABCD a, b, c, d
  SET_ABC \a, \b, \c
  movl \d, %edx
.endm
.macro SET_BC b, c
  movl \b, %ebx
  movl \c, %ecx
.endm
.macro SET_BCD b, c, d
  SET_BC \b, \c
  movl \d, %edx
.endm
.macro SET_CD c, d
  movl \c, %ecx
  movl \d, %edx
.endm

# These set byte values
.macro MOVB_A value
  ZERO %eax
  movb \value, %al
.endm
.macro MOVB_B value
  ZERO %ebx
  movb \value, %bl
.endm
.macro MOVB_C value
  ZERO %ecx
  movb \value, %cl
.endm
.macro MOVB_D value
  ZERO %edx
  movb \value, %dl
.endm

.macro JRZ reg , loc
  // Jump to location if the register is zero.  This uses `test`, which does an
  // AND that only mutates the eflags register.
  test \reg, \reg
  jz \loc
.endm

.macro JRNZ reg, loc
  // Jum to location if the register is not zero.
  test \reg, \reg
  jnz \loc
.endm

# cmp -> jmp is so common that we want to eliminate
# accidentally adding an instruction between them
# (which might alter the flags register).
.macro CMP_JMP cmpType, a, b, jmpCompare, location
  \cmpType \a, \b
  \jmpCompare \location
.endm

.macro JMP_E a, b, location
  CMP_JMP cmp, \a, \b, je, \location
.endm
.macro JMP_NE a, b, location
  CMP_JMP cmp, \a, \b, jne, \location
.endm

# Signed comparisons
.macro JMP_L a, b, location
  CMP_JMP cmp, \a, \b, jl, \location
.endm
.macro JMP_LE a, b, location
  CMP_JMP cmp, \a, \b, jle, \location
.endm
.macro JMP_GE a, b, location
  CMP_JMP cmp, \a, \b, jge, \location
.endm
.macro JMP_G a, b, location
  CMP_JMP cmp, \a, \b, jg, \location
.endm

# Unsigned comparisons
.macro JMP_B a, b, location # jmp below
  CMP_JMP cmp, \a, \b, jb, \location
.endm
.macro JMP_BE a, b, location # jmp below or equal
  CMP_JMP cmp, \a, \b, jbe, \location
.endm
.macro JMP_AE a, b, location # jmp above or equal
  CMP_JMP cmp, \a, \b, jae, \location
.endm
.macro JMP_A a, b, location # jmp above
  CMP_JMP cmp, \a, \b, ja, \location
.endm
.macro MUL4 a
  shll $2, \a # x<<2 is same as x*2^2=x*4
.endm

# Bit Testing
.macro JMP_TESTZ a, b, location
  test \a, \b
  jz \location
.endm
.macro JMP_TESTNZ a, b, location
  test \a, \b
  jnz \location
.endm

return:
  // Functions can conditionally jump here to return
  // I.e.: jnz return
  // instead of having to define a special return memory location
  // within the function.
  ret

sysexit: # %ebx:rc ( -- )
  // Directly call SYS_EXIT using value in ebx
  movl $SYS_EXIT, %eax
  int  $LINUX_SYSCALL

.macro EXIT rc
  movl \rc, %ebx
  jmp sysexit
.endm

asmInvariantViolated: EXIT $RC_ASM_INVARIANT
unreachable: EXIT $RC_UNREACHABLE

// Snapshotting registers allows for debug printing to happen without affecting
// the assembly you are debugging.
.equ offset_eax, 0x00
.equ offset_ebx, 0x04
.equ offset_ecx, 0x08
.equ offset_edx, 0x0C
.equ offset_edi, 0x10
.equ offset_esi, 0x14

.macro SNAPSHOT_REG r
  movl %\r , (registerSnapshot+offset_\r)
.endm

snapshotRegisters:
  SNAPSHOT_REG eax
  SNAPSHOT_REG ebx
  SNAPSHOT_REG ecx
  SNAPSHOT_REG edx
  SNAPSHOT_REG edi
  SNAPSHOT_REG esi
  ret

.macro RESTORE_REG r
  movl (registerSnapshot+offset_\r) , %\r
.endm

restoreRegisters:
  RESTORE_REG eax
  RESTORE_REG ebx
  RESTORE_REG ecx
  RESTORE_REG edx
  RESTORE_REG edi
  RESTORE_REG esi
  ret

syswrite: # %ebx:fd %ecx:&buff %edx:bytesToWrite ( -- )
  # Direct call to SYS_WRITE syscall
  movl $SYS_WRITE, %eax
  int $LINUX_SYSCALL
  ret

syswriteall: # %ebx:fd %ecx:&buff %edx:bytesToWrite ( -- )
  # Writes to the fd, making sure that all bytes are written.
  JMP_NE $0, errno, return
  call syswrite
  JMP_L $0, %eax, syswriteall_error
  subl %eax, %edx # edx=edx-eax  # OPP src, dest
  jz return # if edx=0 we are done
  addl %ecx, %eax
  jmp syswriteall # else, run again
syswriteall_error:
  movl %eax, errno # error
  ret

countntEcx: # %ecx:ntstr ( -- ) %ecx:ntstr %edx:count
  // Count the number of bytes in a null-terminated string
  // Writes the count to edx so it can be used in syswrite
  // uses: ecx, edx
  movl $-1, %edx
countntEcx_loop:
  inc %edx
  cmpb $0, (%ecx, %edx, 1)
  jne countntEcx_loop
  ret

pntEcxNtStr: # %ecx:ntstr ( -- )
  // Print a null terminated string
  // Does not change any registers
  call countntEcx   # edx=bytesToWrite
  movl pntFd, %ebx
  jmp syswriteall

asciiStr: .ascii "~~~~~~~~~\t\n~~\r~~"  # 0x0-F
          .ascii "~~~~~~~~~~~~~~~~"     # 0x10-1F
          .ascii " ! \"$%&'()*+,-./"    # 0x20-2F
          .ascii "0123456789:;<=>?"     # 0x30-3F
          .ascii "@ABCDEFGHIJKLMNO"     # 0x40-4F
          .ascii "PQRSTUVWXYZ[\\]^_"    # 0x50-5F
          .ascii "`abcdefghijklmno"     # 0x60-6F
          .ascii "pqrstuvwxyz{|}~\0"    # 0x70-7E
pntEcxAscii: # %ecx:byte ( -- )
  // Print the ascii value in Ecx
  // writesys requires a pointer so this allows printing single characters.
  movl pntFd, %ebx      # fd
  addl $asciiStr, %ecx  # &buff
  movl $1, %edx         # bytesToWrite
  jmp syswriteall

.macro PNT_ASCII a
  movl \a , %ecx
  call pntEcxAscii
.endm

toasciiEbx: # ( u -- c )
  // Convert a number (typically 1-16) into ascii equivalent
  cmpl $9, %ebx
  jg toasciiEbx_gt9
  # number <= 9
  addl $'0', %ebx
  ret
toasciiEbx_gt9:
  subl $10, %ebx
  cmpl $23, %ebx
  jg asmInvariantViolated # ebx not [0, 24)
  addl $'A', %ebx
  ret

ebxUx: # %ebx:f ( 8o -- 8o ) %edx:right-aligned-count
  // Store ebx as hex ascii into (up to) the first 8 bytes of the stack.
  // The caller must reserve this space.
  // The resulting count is stored in edx so it can be used in syswrite.
  JRZ %ebx, ebxUx_zero
  movl $8, %eax     # %eax=index to store character
  movl %ebx, %ecx   # %ecx=remaining number
ebxUx_loop:
  dec %eax
  JMP_A $7, %eax, asmInvariantViolated  # ja=jump above (unsigned)
  movl %ecx, %ebx
  andl $0x0F, %ebx  # ebx=ebx % 0x10
  call toasciiEbx
  movb %bl, (%ebp, %eax, 1)
  shrl $4, %ecx    # ecx=ecx / 0x10
  jnz ebxUx_loop
  movl $8, %edx
  subl %eax, %edx # return count
  ret
ebxUx_zero:
  movb $'0', 7(%ebp)
  movl $1, %edx
  ret

ebxUxPad: # %ebx:f ( 8o -- 8o ) %edx:count ( always 8 )
  // Store ebx as hex ascii into the first 8 bytes of the stack, pading any
  // remaining bytes.
  // The caller must reserve this space.
  // The resulting count is stored in edx so it can be used in syswrite.
  call ebxUx
  # Set eax=place to start putting zeros
  leal 8(%ebp), %eax
  subl %edx, %eax
  movl $8, %edx # always return count=8
ebxUxPad_loop:
  JMP_LE %ebp, %eax, return
  dec %eax
  movb $'0', (%eax)
  jmp ebxUxPad_loop

pntEbxUx: # %ebx:f ( -- )
  subl $8, %ebp       # reserve space for bytes buffer
  call ebxUx          # edx=bytesToWrite
  movl pntFd, %ebx    # fd
  leal 8(%ebp), %ecx  # &buff=&end-count
  subl %edx, %ecx
  call syswriteall
  addl $8, %ebp       # unreserve space for bytes buffer
  ret

pntEbxUxPretty: # %ebx:f ( -- )
  subl $8, %ebp  # reserve space for bytes buffer
  call ebxUxPad
  movl pntFd, %ebx    # ebx=fd
  movl %ebp, %ecx     # ecx=&buf
  movl $4, %edx       # edx=bytes to write
  call syswriteall
  PNT_ASCII $'_'      # ebx=fd
  leal 4(%ebp), %ecx  # ecx=&buf second half
  movl $4, %edx       # edx=bytes to write
  call syswriteall
  addl $8, %ebp  # unreserve stack
  ret

.macro PNT_NTSTR s
  movl \s, %ecx
  call pntEcxNtStr
.endm
.macro PNT_NTSTR_SPC s
  PNT_NTSTR \s
  PNT_ASCII $' '
.endm
.macro PNT_NTSTR_NL s
  PNT_NTSTR \s
  PNT_ASCII $'\n'
.endm

pntCStr: # %ecx=&cStr
  movl pntFd, %ebx
  MOVB_D (%ecx)
  inc %ecx
  jmp syswriteall

.macro PNT_CSTR cStr
  movl \cStr, %ecx
  call pntCStr
.endm

.macro PNT_CSTR_NL cStr
  PNT_CSTR \cStr
  PNT_ASCII $'\n'
.endm

todoNtStr: .ascii "!! TODO reached !!\n\0"
.macro TODO
  PNT_NTSTR $todoNtStr
  EXIT $42
.endm

.macro PNT_UX v
  movl \v, %ebx
  call pntEbxUx
.endm

.macro PNT_UX_NL v
  PNT_UX \v
  PNT_ASCII $'\n'
.endm

.macro PNT_UX_SPC v
  PNT_UX \v
  PNT_ASCII $' '
.endm

.macro PNT_UX_PRETTY v
  movl \v, %ebx
  call pntEbxUxPretty
.endm

.macro PNT_UX_PRETTY_NL v
  PNT_UX_PRETTY \v
  PNT_ASCII $'\n'
.endm

dbgStartNtStr: .ascii "DBG(\0"
dbgRegNtStr_eax: .ascii " eax=\0"
dbgRegNtStr_ebx: .ascii " ebx=\0"
dbgRegNtStr_ecx: .ascii " ecx=\0"
dbgRegNtStr_edx: .ascii " edx=\0"
dbgRegNtStr_edi: .ascii " edi=\0"
dbgEndNtStr: .ascii ")\n\0"
.macro DBG_REG reg
  PNT_NTSTR $dbgRegNtStr_\reg
  PNT_UX_PRETTY registerSnapshot+offset_\reg
.endm

dbgRegisters: # prints the snapshotted registers
  DBG_REG eax
  DBG_REG ebx
  DBG_REG ecx
  DBG_REG edx
  DBG_REG edi
  ret

.macro DBG v
  // Allows us to debug registers at varioius points
  // Usage: DBG $0x444 will print something like:
  // DBG(0000_0444 eax=0000_0110 ebx=0000_0111 ... edi=0000_0114)
  call snapshotRegisters
  PNT_NTSTR $dbgStartNtStr
  PNT_UX_PRETTY \v
  call dbgRegisters
  PNT_NTSTR $dbgEndNtStr
  call restoreRegisters
.endm

pntMem: # ( start end -- )
  movl (%ebp), %ecx  # start
  movl 4(%ebp), %edx  # end
  JMP_GE %edx, %ecx, pntMem_return
  movl (%ecx), %ebx
  call pntEbxUxPretty
  PNT_ASCII $' '
  addl $4, (%ebp)
  jmp pntMem
pntMem_return:
  addl $8, %ebp
  ret

pstackNtStr: .ascii "PSTACK: \0"
rstackNtStr: .ascii "RSTACK: depth=\0"
pntPstack: # ( -- )
  PNT_NTSTR $pstackNtStr
  movl %ebp, %ebx   # cache ebp (pstack pointer)
  subl $8, %ebp     # make space on stack
  movl %ebx, (%ebp)     # start
  movl pstackMax, %ebx
  movl %ebx, 4(%ebp)    # end
  jmp pntMem

pntRstackDepth: # print the depth of the return stack
  PNT_NTSTR $rstackNtStr
  movl rstackMax, %ebx
  subl %esp, %ebx
  subl $4, %ebx
  shrl $2, %ebx  # ebx=ebx/4
  PNT_UX_NL %ebx
  ret

.macro DBG_STACK v
  // Allows us to debug registers at varioius points
  call snapshotRegisters
  PNT_NTSTR $dbgStartNtStr
  PNT_UX_PRETTY \v
  PNT_ASCII $' '
  call pntPstack
  PNT_NTSTR $dbgEndNtStr
  call restoreRegisters
.endm

panicStartNtStr: .ascii "!! PANIC !! code=0x\0"
panicDefault:
  PNT_NTSTR $panicStartNtStr
  PNT_UX_SPC panicErr
  PNT_NTSTR_NL panicNtStr
  call pntPstack
  PNT_ASCII $'\n'
  call pntRstackDepth
  EXIT $RC_PANIC

.set CURRENT_PANIC_CODE, 0x001
.set PREV_PANIC_ADDR, 0

.macro PANIC_LANDING err
  .equ ERR_\err , CURRENT_PANIC_CODE
  .set CURRENT_PANIC_CODE, CURRENT_PANIC_CODE+1

  // Keep panics in a linked list so we can add them to the dictionary easily.
  .align 4
  errNext_\err : .int PREV_PANIC_ADDR
  .set PREV_PANIC_ADDR, errNext_\err

  errNtStr_\err : .ascii "\err \0"
  panicNow_\err :
    movl $ERR_\err, panicErr
    movl $errNtStr_\err, panicNtStr
    jmp *(panic)
.endm


.macro PANIC err
  jmp panicNow_\err
.endm

PANIC_LANDING OKAY      # used in debugging
PANIC_LANDING ERRNO

panicNow_eaxIsErrno:
  movl %eax, errno
  jmp panicNow_ERRNO
  movl $ERR_ERRNO, panicErr
  jmp *(panic)

checkErrno_str: .ascii "!! errno=\0"
checkErrno:
  // Checks that errno is zero or panics
  // Mutates no registers.
  JMP_E $0, errno, return
  PNT_NTSTR $checkErrno_str
  PNT_UX_NL errno
  jmp panicNow_ERRNO

################################################################################
#### TESTING                                                                ####
################################################################################
// Testing is core to how this tutorial works. This contains utilities for
// testing our code.

testStartNtStr: .ascii "## Test \0"
testFailedNtStr: .ascii "!! Test failed: \0"
startingTest:
  PNT_NTSTR $testStartNtStr
  PNT_NTSTR testingNtStr
  PNT_ASCII $'\n'
  ret

exitFailTest:
  PNT_NTSTR $testFailedNtStr
  PNT_NTSTR testingNtStr
  PNT_ASCII $'\n'
  call pntPstack
  PNT_ASCII $'\n'
  call pntRstackDepth
  EXIT $RC_TESTFAIL

assertFailStr: .ascii "!! ASSERT FAIL: \0"
assertEaxEbxEq_str: .ascii " = \0"
assertEaxEbxEq:
  JMP_E %eax, %ebx, return
  subl $8, %ebp
  movl %eax, (%ebp)
  movl %ebx, 4(%ebp)
  PNT_NTSTR $assertFailStr
  PNT_UX (%ebp)
  PNT_NTSTR $assertEaxEbxEq_str
  PNT_UX_NL 4(%ebp)
  addl $8, %ebp
  jmp exitFailTest

assertEaxEbxNe_str: .ascii " = \0"
assertEaxEbxNe:
  JMP_NE %eax, %ebx, return
  subl $8, %ebp
  movl %eax, (%ebp)
  movl %ebx, 4(%ebp)
  PNT_NTSTR $assertFailStr
  PNT_UX (%ebp)
  PNT_NTSTR $assertEaxEbxNe_str
  PNT_UX_NL 4(%ebp)
  addl $8, %ebp
  jmp exitFailTest

.macro ASSERT_REG_EQ a, b
  SET_AB \a, \b
  call assertEaxEbxEq
.endm

panicExpected:
  // Use this if a panic is expected. It will simply set panic=panicDefault
  // and jmp to panicRestore
  movl $panicDefault, panic
  jmp *(panicRestore)

.macro EXPECT_PANIC l
  movl \l, panicRestore
  movl $panicExpected, panic
.endm

panicErrInvalidStr: .ascii "!! panicErr not expected value: \0"
panicErrInvalid:
  PNT_NTSTR $panicErrInvalidStr
  PNT_UX_SPC panicErr
  PNT_NTSTR_NL $panicNtStr
  jmp exitFailTest

.macro ASSERT_PANICED err
  // Assert that panicErr=err and then clear panicErr
  cmpl \err, panicErr
  jne panicErrInvalid
  movl $0, panicErr
  movl $0, panicNtStr
.endm

assertStackEmptyStr: .ascii "!! Stack not empty: \n\0"
assertStackEmpty:
  movl pstackMax, %eax
  subl %ebp, %eax
  jz return
  PNT_NTSTR $assertStackEmptyStr
  call pntPstack
  PNT_ASCII $'\n'
  call exitFailTest

assertPanicClean_str1: .ascii "!! Not clean panic==panicExpected\n\0"
assertPanicClean_str2: .ascii "!! Not clean panicErr==\0"
assertPanicClean:
  cmpl $panicExpected, panic
  je 1f
  cmpl $0, panicErr
  je return
  PNT_NTSTR $assertPanicClean_str2
  PNT_UX_NL panicErr
  call exitFailTest
1:
  PNT_NTSTR $assertPanicClean_str1
  call exitFailTest

assertClean:
  call checkErrno
  call assertPanicClean
  jmp assertStackEmpty

.macro TESTCASE name
  testname_\name : .ascii "\name\0"
  testcase_\name :
    movl $testname_\name, testingNtStr
    call startingTest
    call assertClean
  testStart_\name :
.endm

TESTCASE pureAsm
  // Test simple assert
  ASSERT_REG_EQ $0x42, $0x42

  // Test panic/expect
  EXPECT_PANIC $1f
  PANIC OKAY
  call unreachable
1:ASSERT_PANICED $ERR_OKAY
  call assertClean

  // test countnt
  movl $asciiStr, %ecx
  call countntEcx
  ASSERT_REG_EQ $0x7F, %edx

  // test toasciiEbx
  .macro TEST_TOASCII v, e
    movl \v, %ebx
    call toasciiEbx
    ASSERT_REG_EQ \e, %ebx
  .endm
  TEST_TOASCII $0, $0x30
  TEST_TOASCII $1, $0x31
  TEST_TOASCII $9, $0x39
  TEST_TOASCII $0xA, $0x41
  TEST_TOASCII $0xF, $0x46

  // test ebxUx and ebxUxPad
  subl $8, %ebp # reserve stack space

  addl $8, %ebp # unreserve stack space

  // Explicit test that stack is empty
  ASSERT_REG_EQ %ebp, pstackMax
  ret


################################################################################
#### STACK                                                                  ####
################################################################################
// Our forth will primarily involve manipulating two stacks, the pstack
// (parameter stack) and rstack (return stack). We have defined pstackMin/Max
// and rstackMin/Max to specify the stack locations, and %ebp is our pstack
// pointer and %esp our rstack pointer.

PANIC_LANDING PSTACK_OVERFLOW
PANIC_LANDING PSTACK_UNDERFLOW
PANIC_LANDING RSTACK_OVERFLOW
PANIC_LANDING RSTACK_UNDERFLOW

.macro INVALID_PUSHP num
invalidPushp\num :
  addl $4*\num, %ebp
  PANIC PSTACK_OVERFLOW
.endm
INVALID_PUSHP 1
INVALID_PUSHP 2
INVALID_PUSHP 3
INVALID_PUSHP 4
INVALID_PUSHP 5

.macro INVALID_POPP num
invalidPopp\num :
  subl $4*\num, %ebp
  PANIC PSTACK_UNDERFLOW
.endm
INVALID_POPP 1
INVALID_POPP 2
INVALID_POPP 3
INVALID_POPP 4
INVALID_POPP 5

.macro INVALID_PUSHR num
invalidPushr\num :
  addl $4*\num, %esp
  PANIC RSTACK_OVERFLOW
.endm
INVALID_PUSHR 1
INVALID_PUSHR 2
INVALID_PUSHR 3

.macro INVALID_POPR num
invalidPopr\num :
  subl $4*\num, %esp
  PANIC RSTACK_UNDERFLOW
.endm
INVALID_POPR 1
INVALID_POPR 2
INVALID_POPR 3

// Set one or more indexes on the stack
.macro SET_P0 c0 # ( c0 )
  movl \c0, (%ebp)
.endm
.macro SET_P1 c0, c1 # ( c0 c1 )
  SET_P0 \c0
  movl \c1, CELL(%ebp)
.endm
.macro SET_P2 c0, c1, c2 # ( c0 c1 c2 )
  SET_P1 \c0, \c1
  movl \c2, 2*CELL(%ebp)
.endm
.macro SET_P3 c0, c1, c2, c3 # ( c0 c1 c2 c3 )
  SET_P2 \c0, \c1, \c2
  movl \c3, 3*CELL(%ebp)
.endm
.macro SET_P4 c0, c1, c2, c3, c4 # ( c0 c1 c2 c3 c4 )
  SET_P3 \c0, \c1, \c2, \c3
  movl \c4, 4*CELL(%ebp)
.endm

// get 1 or more indexes on the stack
.macro GET_P0 c0 # ( c0 ) \ set the index at the stack
  movl (%ebp), \c0
.endm
.macro GET_P1 c0, c1 # ( c0 c1 )
  GET_P0 \c0
  movl CELL(%ebp), \c1
.endm
.macro GET_P2 c0, c1, c2 # ( c0 c1 c2 )
  GET_P1 \c0, \c1
  movl 2*CELL(%ebp), \c2
.endm
.macro GET_P3 c0, c1, c2, c3 # ( c0 c1 c2 c3 )
  GET_P2 \c0, \c1, \c2
  movl 3*CELL(%ebp), \c3
.endm
.macro GET_P4 c0, c1, c2, c3, c4 # ( c0 c1 c2 c3 c4 )
  GET_P3 \c0, \c1, \c2, \c3
  movl 4*CELL(%ebp), \c4
.endm

// Push 1 or more values onto the stack. Note this is NOT indexes.
.macro UNCK_PUSHP c0 # ( -- c0 ) \ unchecked push
  subl $CELL, %ebp
  SET_P0 \c0
.endm
.macro UNCK_PUSHP2 c0, c1 # ( -- c0 c1 )
  subl $2*CELL, %ebp
  SET_P1 \c0, \c1
.endm
.macro UNCK_PUSHP3 c0, c1, c2 # ( -- c0 c1 c2 )
  subl $3*CELL, %ebp
  SET_P2 \c0, \c1, \c2
.endm
.macro UNCK_PUSHP4 c0, c1, c2, c3 # ( -- c0 c1 c2 c3 )
  subl $4*CELL, %ebp
  SET_P3 \c0, \c1, \c2, \c3
.endm
.macro UNCK_PUSHP5 c0, c1, c2, c3, c4 # ( -- c0 c1 c2 c3 c4 )
  subl $5*CELL, %ebp
  SET_P4 \c0, \c1, \c2, \c3, \c4
.endm

// Pop 1 or more values from the stack. Note this is NOT indexes.
.macro UNCK_POPP c0 # ( c0 -- )
  GET_P0 \c0
  addl $CELL, %ebp
.endm
.macro UNCK_POPP2 c0, c1 # ( c0 c1 -- )
  GET_P1 \c0, \c1
  addl $2*CELL, %ebp
.endm
.macro UNCK_POPP3 c0, c1, c2 # ( c0 c1 c2 -- )
  GET_P3 \c0, \c1, \c2
  addl $3*CELL, %ebp
.endm
.macro UNCK_POPP4 c0, c1, c2, c3 # ( c0 c1 c2 c3 -- )
  GET_P3 \c0, \c1, \c2, \c3
  addl $4*CELL, %ebp
.endm

.macro PSTACK_PUSH_PREPARE_AND_CHECK num
  // Moves the pstack down by num cells and checks validity
  subl $CELL*\num, %ebp
  cmpl pstackMin, %ebp
  jb invalidPushp\num
.endm
.macro PUSHP c0 # ( -- c0 ) \ checked push
  PSTACK_PUSH_PREPARE_AND_CHECK 1
  SET_P0 \c0
.endm
.macro PUSHP2 c0, c1 # ( -- c0 c1 )
  PSTACK_PUSH_PREPARE_AND_CHECK 2
  SET_P1 \c0, \c1
.endm
.macro PUSHP3 c0, c1, c2 # ( -- c0 c1 c2 )
  PSTACK_PUSH_PREPARE_AND_CHECK 3
  SET_P2 \c0, \c1, \c2
.endm
.macro PUSHP4 c0, c1, c2, c3 # ( -- c0 c1 c2 c3 )
  PSTACK_PUSH_PREPARE_AND_CHECK 4
  SET_P3 \c0, \c1, \c2, \c3
.endm
.macro PUSHP5 c0, c1, c2, c3, c4 # ( -- c0 c1 c2 c3 c4 )
  PSTACK_PUSH_PREPARE_AND_CHECK 5
  SET_P4 \c0, \c1, \c2, \c3, \c4
.endm

.macro PSTACK_POP_PREPARE_AND_CHECK num
  // Checks that there is enough room in pstack before proceeding
  // Note: we use negative indexing for retrieving the values after
  // this step
  addl $CELL*\num, %ebp
  cmpl pstackMax, %ebp
  ja invalidPopp\num
.endm
.macro POPP c0 # ( c0 -- )
  PSTACK_POP_PREPARE_AND_CHECK 1
  movl -CELL(%ebp),   \c0
.endm
.macro POPP2 c0, c1 # ( c0 c1 -- )
  PSTACK_POP_PREPARE_AND_CHECK 2
  movl -2*CELL(%ebp), \c0
  movl -CELL(%ebp),   \c1
.endm
.macro POPP3 c0, c1, c2 # ( c0 c1 c2 -- )
  PSTACK_POP_PREPARE_AND_CHECK 3
  movl -3*CELL(%ebp), \c0
  movl -2*CELL(%ebp), \c1
  movl -CELL(%ebp),   \c2
.endm
.macro POPP4 c0, c1, c2, c3 # ( c0 c1 c2 c3 -- )
  PSTACK_POP_PREPARE_AND_CHECK 4
  movl -4*CELL(%ebp), \c0
  movl -3*CELL(%ebp), \c1
  movl -2*CELL(%ebp), \c2
  movl -CELL(%ebp),   \c3
.endm

.macro DROPP n
  addl $CELL*\n , %ebp
  cmpl pstackMax, %ebp
  jg invalidPopp\n
.endm

.macro DROPR n
  addl $CELL*\n , %esp
  cmpl rstackMax, %esp
  jg invalidPopr\n
.endm

drop:
  cmpl pstackMax, %ebp
  jge panicNow_PSTACK_UNDERFLOW
  addl $CELL, %ebp
  ret

assertEq: # ( a b -- )
  POPP %eax
  POPP %ebx
  jmp assertEaxEbxEq

.macro ASSERT_TOP v
  PUSHP \v
  call assertEq
.endm

.macro ASSERT_FAILED_LANDING name
  assertFailedStr\name : .ascii "\name\0"
  assertFailed\name :
    call snapshotRegisters
    PNT_NTSTR_SPC $assertFailStr
    PNT_NTSTR_NL $assertFailedStr\name
    call dbgRegisters
    PNT_ASCII $'\n'
    call restoreRegisters
    jmp exitFailTest
.endm

ASSERT_FAILED_LANDING ASSERT_AB
.macro ASSERT_AB a, b
  JMP_NE \a, %eax, assertFailedASSERT_AB
  JMP_NE \b, %ebx, assertFailedASSERT_AB
.endm
ASSERT_FAILED_LANDING ASSERT_ABC
.macro ASSERT_ABC a, b, c
  JMP_NE \a, %eax, assertFailedASSERT_ABC
  JMP_NE \b, %ebx, assertFailedASSERT_ABC
  JMP_NE \c, %ecx, assertFailedASSERT_ABC
.endm
ASSERT_FAILED_LANDING ASSERT_ABCD
.macro ASSERT_ABCD a, b, c, d
  JMP_NE \a, %eax, assertFailedASSERT_ABCD
  JMP_NE \b, %ebx, assertFailedASSERT_ABCD
  JMP_NE \c, %ecx, assertFailedASSERT_ABCD
  JMP_NE \d, %edx, assertFailedASSERT_ABCD
.endm

TESTCASE pstack
  PUSHP $0x42
  PUSHP $0x4422
  ASSERT_TOP $0x4422
  ASSERT_TOP $0x42
  EXPECT_PANIC $1f
  POPP %eax # pop from empty stack
  call unreachable
1:ASSERT_PANICED $ERR_PSTACK_UNDERFLOW
  call assertStackEmpty

  movl pstackMin, %ebp # mimick full stack
  EXPECT_PANIC $1f
  PUSHP $0x42
  call unreachable
1:ASSERT_PANICED $ERR_PSTACK_OVERFLOW
  movl pstackMax, %ebp

  PUSHP2 $1, $2
  ASSERT_TOP $1
  ASSERT_TOP $2

  PUSHP3 $1, $2, $3
  ASSERT_TOP $1
  ASSERT_TOP $2
  ASSERT_TOP $3

  PUSHP4 $1, $2, $3, $4
  ASSERT_TOP $1
  ASSERT_TOP $2
  ASSERT_TOP $3
  ASSERT_TOP $4

  PUSHP2 $1, $2
  POPP2 %eax, %ebx
  ASSERT_AB $1, $2

  PUSHP3 $3, $4, $5
  POPP3 %eax, %ebx, %ecx
  ASSERT_ABC $3, $4, $5

  PUSHP4 $6, $7, $8, $9
  POPP4 %eax, %ebx, %ecx, %edx
  ASSERT_ABCD $6, $7, $8, $9
  jmp assertClean

pushRsInvalid:
  # Checks to make sure that the datastack hasn't overflowed.
  # If it has, re-increments and panics with appropriate error.
  addl $CELL, %esp
  PANIC RSTACK_OVERFLOW

.macro PUSHR reg
  pushl \reg
  cmpl rstackMin, %esp
  jl pushRsInvalid
.endm

popRsInvalid:
  # Checks to make sure that the datastack hasn't underflowed.
  # If it has, re-decrements and panics with appropriate error.
  subl $CELL, %esp
  PANIC RSTACK_UNDERFLOW

.macro POPR reg
  popl \reg
  cmpl rstackMax, %esp
  jge popRsInvalid
.endm

.macro GET_P index, reg
  movl 4*\index(%ebp), \reg
.endm
.macro SET_P index, reg
  movl \reg, 4*\index(%ebp)
.endm

.macro CHECK_PSTACK_INDEX indexReg
  // Check that the pstack index is valid. WARN: modifes indexReg
  shll $2, \indexReg # multiply by 4
  addl \indexReg, %ebp
  cmpl pstackMax, \indexReg
  ja panicNow_PSTACK_UNDERFLOW
.endm

################################################################################
#### Byte comparison and movement functions                                 ####
################################################################################

.equ LT, -1
.equ EQ, 0
.equ GT, 1

bcmp: # %eax:&a %ebx:aCount %ecx:&b %edx:bCount ( -- ) %eax:cmp
  // Compares bytes at address &a to &b. Returns:
  // eax=-1 if a<b
  // eax=0 if a==b
  // eax=1 if a>b
  // "a is less than b " is defined as `a` having a byte with a lesser ascii
  // value OR (if no such byte is found) fewer count.
  // Uses: %edi
  PUSHP %ebx
  ZERO %edi
bcmp_loop:
  JMP_GE (%ebp), %edi, bcmp_aCountExhausted
  JMP_GE %edx, %edi, bcmp_gt # jmp if b was exhausted, a wasn't, therefore a>b
  movb (%eax,%edi,1), %bl
  cmpb (%ecx,%edi,1), %bl # cmp b, a
  jl bcmp_lt
  jg bcmp_gt
  inc %edi
  jmp bcmp_loop
bcmp_aCountExhausted:
  JMP_GE %edx, %edi, bcmp_eq # eq if bCount was also exhausted
  jmp bcmp_lt # else a<b
bcmp_eq:
  ZERO %eax # EQ
  jmp drop
bcmp_gt:
  movl $GT, %eax
  jmp drop
bcmp_lt:
  movl $LT, %eax
  jmp drop

.macro ASSERT_BYTES_CMP cmp, a, aCount, b, bCount
  SET_ABCD \a, \aCount, \b, \bCount
  call bcmp
  ASSERT_REG_EQ %eax, \cmp
.endm

test_bcmpStr: .ascii "aabbcc\0"
TESTCASE bcmp
  ASSERT_BYTES_CMP $EQ, $test_bcmpStr,   $3, $test_bcmpStr   ,$3 # same string is equal
  ASSERT_BYTES_CMP $LT, $test_bcmpStr,   $2, $test_bcmpStr   ,$3 # less count is less than
  ASSERT_BYTES_CMP $LT, $test_bcmpStr,   $5, $test_bcmpStr+1 ,$3 # a<b
  ASSERT_BYTES_CMP $GT, $test_bcmpStr+4, $2, $test_bcmpStr   ,$3 # c>a
  ret

.macro TO_UPPER v
  # Convert ascii byte from lower->upper case.
  # Notes: 'A'=0x41 'Z'=0x5A 'a'=0x61 'z'=0x7A
  cmpb $'a', \v
  jl return
  cmpb $'z', \v
  jg return
  # else subtract 0x20 to convert to upper case.
  subb $0x20, \v
  ret
.endm

alToUpper: TO_UPPER %al
ahToUpper: TO_UPPER %ah
blToUpper: TO_UPPER %bl

beqNocase: # %eax:&a %ebx:aCount %ecx:&b %edx:bCount ( -- ) %eax:bool
  // Compare byte sequence a to b for equality, ignoring case
  JMP_NE %ebx, %edx, beqNocase_neq
  movl %eax, %ebx  # ebx:&a
  ZERO %edi
beqNocase_loop:
  JMP_GE %edx, %edi, beqNocase_eq
  movb (%edi, %ebx, 1), %al
  movb (%edi, %ecx, 1), %ah
  call alToUpper
  call ahToUpper
  inc %edi
  cmpb %al, %ah
  je beqNocase_loop # if eq continue loop, else not equal
beqNocase_neq:
  movl $FALSE, %eax
  ret
beqNocase_eq:
  movl $TRUE, %eax
  ret

.macro ASSERT_BYTES_NOCASE_EQ aCount, a, bCount, b
  SET_ABCD \a, \aCount, \b, \bCount
  call beqNocase
  ASSERT_REG_EQ %eax, $TRUE
.endm

.macro ASSERT_BYTES_NOCASE_NE aCount, a, bCount, b
  SET_ABCD \a, \aCount, \b, \bCount
  call beqNocase
  ASSERT_REG_EQ %eax, $FALSE
.endm

test_beqNocaseStr: .ascii "aaAAbbBB\0"
TESTCASE beqNocase
  ASSERT_BYTES_NOCASE_EQ $3, $test_beqNocaseStr, $3, $test_beqNocaseStr   # same string =
  ASSERT_BYTES_NOCASE_EQ $2, $test_beqNocaseStr, $2, $test_beqNocaseStr+2 # different case =
  ASSERT_BYTES_NOCASE_NE $3, $test_beqNocaseStr, $2, $test_beqNocaseStr   # different lengths !=
  ASSERT_BYTES_NOCASE_NE $2, $test_beqNocaseStr, $2, $test_beqNocaseStr+3 # different letter !=
  ret

bmove: # %eax:count %ebx:&from %ecx:&to ( -- )
  // Moves count bytes from &from to &to starting at the beginning.
  // Uses registers a-d + edi
  ZERO %edi
bmove_loop:
  JMP_GE %eax, %edi, return
  movb (%ebx,%edi,1), %dl
  movb %dl, (%ecx,%edi,1)
  inc %edi
  jmp bmove_loop

cmove: # %eax:count %ebx:&from %ecx:&to ( -- )
  // Moves count cells from &from to &to starting at beginning
  // Uses registers a-d + edi
  ZERO %edi
cmove_loop:
  JMP_GE %eax, %edi, return
  movl (%ebx, %edi, 4), %edx
  movl %edx, (%ecx, %edi, 4)
  inc %edi
  jmp cmove_loop

.macro BMOVE count, from, to
  SET_ABC \count, \from, \to
  call bmove
.endm

cStrMove: # %ebx:&fromCstr &ecx:&toCstr
  MOVB_A (%ebx)      # eax = count of from
  movb %al, (%ecx)   # set `to` to same count
  inc %ebx  # inc both to point at bytes (not count)
  inc %ecx
  jmp bmove

.macro CSTR_MOVE from, to
  SET_BC \from, \to
  call cStrMove
.endm

cmpCStr: # %eax:&aCStr %ecx:&bCStr ( -- ) %eax:cmp
  MOVB_B (%eax)
  inc %eax
  MOVB_D (%ecx)
  inc %ecx
  jmp bcmp

cStrEqNocase: # %eax:&aCStr %ecx:&bCStr
  MOVB_B (%eax)
  inc %eax
  MOVB_D (%ecx)
  inc %ecx
  jmp beqNocase

.macro ASSERT_CSTR_NOCASE_EQ aCStr, bCStr
  movl \aCStr, %eax
  movl \bCStr, %ecx
  call cStrEqNocase
  ASSERT_REG_EQ %eax, $TRUE
.endm

test_bmoveCStr: .byte 0x06
  .ascii "abcDEz"
test_bmoveCStr2: .byte 0x03
  .ascii "p12"
TESTCASE bmove
  subl $0x20, %ebp
  CSTR_MOVE $test_bmoveCStr, %ebp
  ASSERT_CSTR_NOCASE_EQ $test_bmoveCStr, %ebp
  CSTR_MOVE $test_bmoveCStr2, %ebp
  ASSERT_CSTR_NOCASE_EQ $test_bmoveCStr2, %ebp
  addl $0x20, %ebp
  ret

fzero: # %eax:count %ebx:&fyte
  // Zero memory in fyte sizes
  JRZ %eax, return
  dec %eax
  movl $0, (%ebx, %eax, 4)
  jmp fzero

################################################################################
#### Simple Pure Assembly Utility Functions and Macros                      ####
################################################################################
.macro ALIGN4 reg
  // modify reg so it is aligned by 4
  addl $3, \reg   # Add 3 to the number, pushing it up to or over 4byte boundary
                  # iff not aligned
  andl $~3, \reg  # Same as %4 (but faster). Gets rid of two lowest bytes.
.endm

.macro TEST_ALIGN4 before, expected
  movl \before, %eax
  ALIGN4 %eax
  movl \expected, %ebx
  call assertEaxEbxEq
.endm
TESTCASE align4
  TEST_ALIGN4 $0, $0
  TEST_ALIGN4 $1, $4
  TEST_ALIGN4 $2, $4
  TEST_ALIGN4 $3, $4
  TEST_ALIGN4 $4, $4
  TEST_ALIGN4 $0x4000, $0x4000
  TEST_ALIGN4 $0x4001, $0x4004
  TEST_ALIGN4 $0x4004, $0x4004
  ret

sllPush: # %eax:&node (or &root) %ebx:&newNode ( -- )
  // Singly-linked-list: push newNode to be after node
  // uses: eax, ebx, ecx
  // Before: node -> nextNode
  // After:  node -> newNode -> nextNode
  movl (%eax), %ecx  # ecx=node.&next
  movl %ecx, (%ebx)  # newNode.&next=node.&next
  movl %ebx, (%eax)  # node.&next=&newNode
  ret

sllPop: # %eax:&prev &ebx:&node
  // pop node
  // Before: prev -> node -> nextNode
  // After: prev -> nextNode
  movl (%ebx), %ecx  # ecx=node.&next
  movl %ecx, (%eax)  # set prev.&next = node.&next
  movl $0, (%ebx)    # clear node.&next
  ret

.macro SLL_PUSH node, newNode
  SET_AB \node, \newNode
  call sllPush # a -> b
.endm
.macro SLL_POP prevNode, node
  SET_AB \prevNode, \node
  call sllPop
.endm

TESTCASE sll
  movl $0, testSpace_00 # 00 (&root) starts at null
  movl $0xDeadBeef, testSpace_04 # 04 starts at something else
  SLL_PUSH $testSpace_00, $testSpace_04
  ASSERT_REG_EQ (testSpace_00), $testSpace_04
  ASSERT_REG_EQ (testSpace_04), $0
  SLL_PUSH $testSpace_00, $testSpace_08
  ASSERT_REG_EQ (testSpace_00), $testSpace_08
  ASSERT_REG_EQ (testSpace_08), $testSpace_04
  ASSERT_REG_EQ (testSpace_04), $0

  SLL_POP $testSpace_00, $testSpace_08
  ASSERT_REG_EQ (testSpace_08), $0
  ASSERT_REG_EQ testSpace_00, $testSpace_04
  ASSERT_REG_EQ testSpace_04, $0
  ret

PANIC_LANDING INVALID_HEX_AL

blToHex:
  call blToUpper
  cmpb $'9', %bl
  jbe blToHex_be9
  # above 9
  cmpb $'F', %bl
  ja panicNow_INVALID_HEX_AL
  # between 9 and F
  cmpb $'A', %bl
  jb panicNow_INVALID_HEX_AL
  subb $'A'-10, %bl
  ret
blToHex_be9:
  cmpb $'0', %bl
  jb panicNow_INVALID_HEX_AL
  subb $'0', %bl
  ret

PANIC_LANDING INVALID_NUM_OVERFLOW
PANIC_LANDING INVALID_NUM_LEN
asciiToHex: # %ecx:&b %edx:count ( -- ) %eax:value
  // Convert ascii values to hexadecimal
  JRZ %edx, panicNow_INVALID_NUM_LEN
  addl %ecx, %edx # edx=end-addr
  ZERO %eax
  ZERO %ebx
asciiToHex_loop:
  JMP_E %ecx, %edx, return
  movb (%ecx), %bl
  cmpb $'_', %bl
  je asciiToHex_skip      # ignore underscores
  # panic if shift will result in "overflow"
  JMP_TESTNZ $0xF0000000, %eax, panicNow_INVALID_NUM_OVERFLOW
  shl $4, %eax    # eax=eax*16
  call blToHex
  addl %ebx, %eax
asciiToHex_skip:
  inc %ecx
  jmp asciiToHex_loop

.macro TEST_ASCII_TO_HEX asciiAddr, count, expected
  BMOVE \count, \asciiAddr, %ebp
  SET_CD %ebp, \count
  call asciiToHex
  ASSERT_REG_EQ %eax, \expected
.endm
test_asciiToHex_0: .ascii "0"
test_asciiToHex_42: .ascii "42"
test_asciiToHex_3aB03: .ascii "3aB03"
test_asciiToHex_DeadBeef: .ascii "DeadBeef"
test_asciiToHex_2002_1111: .ascii "2002_1111"
test_asciiToHex_1_0000_0000: .ascii "1_0000_0000"
TESTCASE asciiToHex
  subl $0x10, %ebp
  TEST_ASCII_TO_HEX $test_asciiToHex_0,           $0x1, $0x00000000
  TEST_ASCII_TO_HEX $test_asciiToHex_42,          $0x2, $0x00000042
  TEST_ASCII_TO_HEX $test_asciiToHex_3aB03,       $0x5, $0x0003aB03
  TEST_ASCII_TO_HEX $test_asciiToHex_DeadBeef,    $0x8, $0xDeadBeef
  TEST_ASCII_TO_HEX $test_asciiToHex_2002_1111,   $0x9, $0x20021111

  EXPECT_PANIC $1f
  TEST_ASCII_TO_HEX $test_asciiToHex_1_0000_0000, $0xB, $0xD1E
1:ASSERT_PANICED $ERR_INVALID_NUM_OVERFLOW
  // handle extra item on the return stack (never returned from asciiToHex)
  DROPR 1

  EXPECT_PANIC $1f
  SET_CD $0, $0
  call asciiToHex
1:ASSERT_PANICED $ERR_INVALID_NUM_LEN
  DROPR 1 # handle never returning from asciiToHex
  addl $0x10, %ebp
  ret

PANIC_LANDING INVALID_BINARY
asciiToBinary: # %ecx:&b %edx:count ( -- ) %eax:value
  // Convert ascii values to binary
  JRZ %edx, panicNow_INVALID_NUM_LEN
  addl %ecx, %edx # edx=end-addr
  ZERO %eax
  ZERO %ebx
asciiToBinary_loop:
  JMP_E %ecx, %edx, return
  movb (%ecx), %bl
  cmpb $'_', %bl
  je asciiToBinary_skip      # ignore underscores
  # panic if shift will result in "overflow"
  JMP_TESTNZ $0x80000000, %eax, panicNow_INVALID_NUM_OVERFLOW
  shl $1, %eax    # eax=eax*2
  subb $'0', %bl
  cmpb $1, %bl
  ja panicNow_INVALID_BINARY
  addl %ebx, %eax
asciiToBinary_skip:
  inc %ecx
  jmp asciiToBinary_loop


################################################################################
#### Reading words from source                                              ####
################################################################################
# This is extremely important for obvious reasons :D

sysopen: # %ebx:&fnameNt %ecx:intention %edx:permissions ( -- ) %eax:fd
  # Open the nt filename.
  # check %eax for errors and panic if they exist.
  movl $SYS_OPEN, %eax
  int $LINUX_SYSCALL
  ret

sysclose: # %ebx:fd ( -- ) 
  # Close the filedescriptor
  movl $SYS_CLOSE, %eax
  int $LINUX_SYSCALL
  ret

.macro OPEN_SOURCE_FD filepathNt
  SET_BCD \filepathNt, $O_RDONLY, $0666 # edx=permissions
  call sysopen
  JMP_L $0, %eax, panicNow_eaxIsErrno
  movl %eax, sourceFd
.endm
.macro CLOSE_SOURCE_FD
  movl sourceFd, %ebx
  call sysclose
  JMP_L $0, %eax, panicNow_eaxIsErrno
  movl $STDIN, sourceFd
  movb $0, sourceEof  # reset source Eof
  movb $0, sourceCStr # reset source Count
.endm

sysread: # %ebx:fd %ecx:&buff %edx:maxLength ( -- ) %eax:numRead
  # eax=0 for EOF and negative for error
  movl $SYS_READ, %eax
  int $LINUX_SYSCALL
  ret

findWhitespaceStart: # %eax:count %ebx:&bytes ( -- ) %eax:count_remain %ebx:&whitespace
  # Find whitespace. %ebx will be zero if there was none, else it will be a pointer
  # to the whitespace.
  JRZ %eax, findWhitespaceStart_none
  // all bytes > ascii-space are not whitespace. This means utf8 is supported
  // out of the box, since all non-ascii utf8 bytes are > 127
  cmpb $' ', (%ebx)
  jbe return # be="Below or Equal", unsigned variant of le
  dec %eax
  inc %ebx
  jmp findWhitespaceStart
findWhitespaceStart_none:
  movl $0, %ebx
  ret

findWhitespaceEnd: # %eax:count %ebx:&bytes ( -- ) %eax:count_remain %ebx:&not_whitespace
  JRZ %eax, findWhitespaceEnd_none
  cmpb $' ', (%ebx)
  ja return # ja=Jmp Above
  dec %eax
  inc %ebx
  jmp findWhitespaceEnd
findWhitespaceEnd_none:
  movl $0, %ebx
  ret

fillSourceCStr:
  # Attempt to fill the sourceStr buffer.
  # Modifies sourceEof, sourceCStr, sourceStr with the result
  MOVB_C sourceCStr     # count of sourceStr
  leal sourceStr(%ecx), %ecx # ecx=&buff, end of sourceStr space
  movl $SOURCE_STR_CNT_MAX, %edx
  subb sourceCStr, %dl  # edx=countToRead
  movl sourceFd, %ebx   # ebx=Fd
  call sysread
  JMP_L $0, %eax, panicNow_eaxIsErrno
  JRZ %eax, readSource_eof
  addb %al, sourceCStr # add number read to the count
  ret
readSource_eof:
  movb $TRUE, sourceEof
  ret

flush_sourceCStr_to_wordCStr:
  // %eax:count ( -- )
  // Does two things:
  // - Moves the word of length %eax into wordCStr
  // - Moves the remaining (non-whitespace) bytes to beginning of sourceCStr
  //   and updates count
  movb %al, wordCStr  # update wordCnt
  movl $sourceStr, %ebx # &from
  movl $wordStr, %ecx # &to
  call bmove # note: eax already=count

  MOVB_A sourceCStr  # eax will = count leftover in sourceCStr
  subb wordCStr, %al   # eax=sourceCount - wordCount
  MOVB_B wordCStr
  leal sourceStr(%ebx), %ebx # ebx=&start_of_whitespace
  call findWhitespaceEnd
  movb %al, sourceCStr  # update count
  # eax=count-after-non-whitespace, ebx=&after-non-whitespace
  movl $sourceStr, %ecx # ecx=&to
  jmp bmove

PANIC_LANDING MAX_WORD_CNT

word: # Store the next word from the source into wordCStr
  call fillSourceCStr

  MOVB_A sourceCStr # eax=sourceCount
  JRZ %eax, word_noWhitespace # if count=0, check Eof and try again
  movl $sourceStr, %ebx
  call findWhitespaceStart # note: eax:count_remain %ebx:&whitespace
  JRZ %ebx, word_noWhitespace

  # Found a word, update wordCStr and sourceCStr and return
  movl %eax, %ebx # eax will = wordCount
  MOVB_A sourceCStr
  subl %ebx, %eax
  jmp flush_sourceCStr_to_wordCStr
word_noWhitespace:
  // There can be no whitespace for a few reasons:
  // - We reached the EOF, meaning this is the last word -- return whatever we
  //   have (may be empty) 
  // - We simply didn't read enough bytes, try again.
  // - The word was too large to fit in the buffer, this is an error.
  cmpb $0, sourceEof
  jne word_eof
  cmpb $WORD_CNT_MAX, sourceCStr
  // jbe: didn't read enough bytes, try again. Note, if equal may be
  // because we reached EOF, but we won't know until we try again.
  jbe word
  PANIC MAX_WORD_CNT
word_eof: # eof reached, return what we have
  MOVB_A sourceCStr
  jmp flush_sourceCStr_to_wordCStr

.macro TEST_FIND_WHITESPACE fn, cStr, expectedCountRemaining
  # Test findWhitespace using the cStr and the count
  # First, assert expectedCountRemaining is correct.
  MOVB_A \cStr
  movl $\cStr+1, %ebx
  call \fn
  movl %ebx, %ecx # cache ebx for next assert
  movl $\expectedCountRemaining, %ebx
  call assertEaxEbxEq

  # Next, assert the pointer points to the right memory
  movb \cStr, %al  # total count
  subl $\expectedCountRemaining, %eax # eax=size of whitespace
  addl $\cStr, %eax # eax=expected pointer location
  inc %eax # inc because of the count byte in cStr
  movl %ecx, %ebx # load cached ebx
  call assertEaxEbxEq
.endm

test_wordCStr1: .byte 0xC
test_wordStr1: .ascii "the    quick"
test_wordCStr2: .byte 8
test_wordStr2: .ascii "   quick"
TESTCASE findWhitespace
  TEST_FIND_WHITESPACE findWhitespaceStart, test_wordCStr1, 9 # note: 9 REMAIN
  TEST_FIND_WHITESPACE findWhitespaceStart, test_wordCStr2, 8
  TEST_FIND_WHITESPACE findWhitespaceEnd, test_wordCStr1, 0xC
  TEST_FIND_WHITESPACE findWhitespaceEnd, test_wordCStr2, 5
  ret

file_some_words: .ascii "tests/some_words.txt\0"
file_some_words_cStr: .byte 0x46
  .ascii "the     quick\n\n\t\t    \tbrown\n  fox jumped over    the "
  .ascii "lazy    dog\n\nthe "
test_wordCStr_specialFill: .byte 0x46
  .ascii "the     quick\n\n\t\t    \tbrown\n  fox jumped over    the "
  .ascii "lazy    dog\n\n    "
TESTCASE fillSourceCStr
  OPEN_SOURCE_FD $file_some_words
  call fillSourceCStr # do a normal fill, assert as expected
  ASSERT_CSTR_NOCASE_EQ $sourceCStr, $file_some_words_cStr
  # now fill only 4 characters
  movb $0x42, sourceCStr  # 0x46 - 4
  call fillSourceCStr # do a normal fill, assert as expected
  ASSERT_CSTR_NOCASE_EQ $sourceCStr, $test_wordCStr_specialFill
  CLOSE_SOURCE_FD
  ret

TESTCASE flush_sourceCStr_to_wordCStr
  CSTR_MOVE $test_wordCStr1, $sourceCStr
  movl $3, %eax
  call flush_sourceCStr_to_wordCStr # moves "the" to stack, keeps "quick"
  movl $wordStr, %ebx
  ASSERT_BYTES_CMP $EQ, %ebx, $3,$test_wordStr1, $3 # stack = "the"
  MOVB_A wordCStr # eax=wordCnt
  ASSERT_REG_EQ %eax, $3
  ASSERT_BYTES_CMP $EQ, $sourceStr, $5, $test_wordStr1+7, $5  # remaining source="quick"
  movb $0, sourceEof  # reset source Eof
  movb $0, sourceCStr # reset source Count
  ret

test_wordWordsNtStr:
  .ascii "the\0quick\0brown\0fox\0jumped\0over\0the\0lazy\0dog\0"
  .ascii "the\0quick\0brown\0fox\0jumped\0over\0the\0lazy\0dog\0\0"
TESTCASE word
  PUSHR $test_wordWordsNtStr # (%esp)=expected word
  OPEN_SOURCE_FD $file_some_words
test_words:
  call word # get word from sourceFd
  # Get expected word
  movl (%esp), %ecx
  call countntEcx    # ecx=ntstr edx=count
  # Update expected word for next loop (%esp) = (%exp) + %edx + 1
  addl %edx, (%esp)
  addl $1, (%esp)
  # Assert expected = found
  MOVB_B wordCStr     # ebx=wordCnt
  movl $wordStr, %eax # eax=&word
  call bcmp
  ASSERT_REG_EQ %eax, $EQ
  JRNZ %edx, test_words  # if the word len != 0, loop again
test_words_end:
  CLOSE_SOURCE_FD
  POPR %eax
  ret

################################################################################
#### Forth Dictionary                                                       ####
################################################################################
// Our forth dictionary is going to be unlike most implementations. Primary
// differences:
// - Dictionary metadata such as the ascii representation of the word,
//   immediacy, and type information (we'll get to that) are in a separate
//   memory region from the code. They can be dropped when "compilation" is
//   complete to save memory.
// - There are really 3 dictionaries:
//   1. Runtime: this is the "standard" one containing forth words that can be
//      run at runtime. Runtime words should never depend on any other
//      dictionary's words (and this is maintained by execOrCompile).
//   2. Compiletime: this is a separate dictionary of words which can be
//      completely discarded (code AND metadata) when "compilation" is
//      complete. Compiletime words may depend on runtime words but not
//      localtime words.
//   3. Localtime: yet another dictionary containing words (typically
//      immediate or debugging) for use within a function. Can be used to make one-off macros
//      for helpful variable naming, etc.

# Bitmap for info0
.equ INFO0_IMM, 0x10000000

.equ DICT_SIZE_WITHOUT_NAME, 3 * 4
.equ DICT_FIELD_nextAddr, 0x0
.equ DICT_FIELD_xt,       0x4
.equ DICT_FIELD_info0,    0x8
.equ DICT_FIELD_nameCStr, 0xC
.macro DICT_META_GET_ADDR metaAddr, field, reg
  movl \metaAddr, \reg
  addl $DICT_FIELD_\field, \reg
.endm
.macro DICT_META_GET metaAddr, field, reg
  DICT_META_GET_ADDR \metaAddr, \field, \reg
  movl (\reg), \reg
.endm
.macro DICT_META_SET metaAddr, field, value, reg
  # Set value to field. Use reg as temporary register.
  DICT_META_GET_ADDR \metaAddr, \field, \reg
  movl \value, (\reg)
.endm

.macro SETUP_DICT_CODE type, defaultSize
  movl heap, %eax
  movl %eax, dictCodeMin\type
  movl %eax, dictCodeHere\type
  addl \defaultSize, %eax
  movl %eax, dictCodeMax\type
  movl %eax, heap
.endm
.macro SETUP_DICT_META type, defaultSize
  movl heap, %eax
  movl %eax, dictMetaMin\type
  movl %eax, dictMetaHere\type
  addl \defaultSize, %eax
  movl %eax, dictMetaMax\type
  movl %eax, heap
.endm
.equ DEFAULT_DICT_SECTION_SIZE, 0x40000
.equ DEFAULT_LTIME_SIZE,        0x3000 # 12 KiB
dictSetup:
  SETUP_DICT_CODE RTime,  $DEFAULT_DICT_SECTION_SIZE
  SETUP_DICT_CODE CTime,  $DEFAULT_DICT_SECTION_SIZE

  SETUP_DICT_META RTime,      $DEFAULT_DICT_SECTION_SIZE
  SETUP_DICT_META CTime,  $DEFAULT_DICT_SECTION_SIZE
  movl $DEFAULT_DICT_BUCKETS_CNT-1, dictBucketsAndValue
  movl heap, %eax
  movl %eax, dictBucketsAddr
  addl $DEFAULT_DICT_BUCKETS_CNT*4, heap
  movl $DEFAULT_DICT_BUCKETS_CNT, %eax
  movl dictBucketsAddr, %ebx
  call fzero

  movl heap, %eax
  movl %eax, dictLTimeMin
  addl $DEFAULT_LTIME_SIZE, %eax
  movl %eax, dictLTimeMax
  jmp dictResetLTime

dictResetLTime:
  // The Localtime dict can be completely reset quickly.
  movl dictLTimeMin, %eax
  movl %eax, dictLTimeHere
  movl $0, dictLTimeRoot
  ret

pjwHashNoCaseEax: # %ecx:&bytes %edx:count  ( -- ) %edi:hash
  // Do a PJW hash (ignoring case) on count bytes.
  // https://en.wikipedia.org/wiki/PJW_hash_function
  ZERO %edi
  addl %ecx, %edx # edx=end addr
pjwHashEax_loop:
  JMP_AE %edx, %ecx, return
  # Next 4 lines: h = (h << 4) + *s++
  shll $4, %edi
  MOVB_A (%ecx)
  call alToUpper
  addl %eax, %edi
  inc %edi
  movl %edi, %ebx  # ebx will = high bits below
  andl $0xF0000000, %ebx
  JRZ %ebx, 1f # if(high)
  # Next three lines are: h ^= high >> 24
  movl %ebx, %eax # eax is used as temprary, need to keep high
  shrl $24, %eax
  xorl %eax, %edi
1:
  # Next two lines are: h &= ~high
  notl %ebx
  andl %ebx, %edi
  inc %ecx
  jmp pjwHashEax_loop

dictCStrBucketAddr: # %ecx:&cStr ( -- ) %edi:&root
  // Note: &root isn't _really_ a node (has no data) but acts like a node for
  // sll* methods since the first item is a pointer to a node.
  MOVB_D (%ecx)
  inc %ecx # ecx=&str
  call pjwHashNoCaseEax
  andl dictBucketsAndValue, %edi
  MUL4 %edi
  addl dictBucketsAddr, %edi
  ret

.equ DICT_TYPE_RTIME, 0 # runtime
.equ DICT_TYPE_CTIME, 1 # compiletime
getDictTypeGlobalAddr: # %eax:dictType ( -- ) %eax:dictGlobalAddr
  JRZ %eax, dictAddr_RTime
  JMP_E $DICT_TYPE_CTIME, %eax, dictAddr_CTime
  jmp asmInvariantViolated
.macro dictAddr_ type
  dictAddr_\type :
    movl $dict\type , %eax
    ret
.endm
dictAddr_ RTime
dictAddr_ CTime

dictPush: # %eax:dictTypeGlobalAddr %ecx:&nameCStr ( -- ) %eax:&meta (aka &newNode)
  PUSHP %ecx # stack: ( &nameCStr )
  DICT_GLOBAL_GET_ADDR META_HERE, %eax # %eax=&metaHere
  movl (%eax), %edx # edx=metaHere=&newNode
  # Calculate new metaHere via ecx=newNodeSize
  GET_P 3, %ebx # ebx=&wordCStr
  MOVB_C (%ebx) # ecx=wordCnt
  inc %ecx      # plus Cnt byte
  addl %edx, %ecx # plus original metaHere
  addl $DICT_SIZE_WITHOUT_NAME, %ecx # plus normal size of dict
  // TODO: check META_MAX
  movl %ecx, (%eax)
  // Set initial fields in new entry (info0, nameCStr). xt is set by dictNew
  DICT_META_SET %edx, info0, $0, %eax # inital info0=0
  DICT_META_GET_ADDR %edx, nameCStr, %ecx  # %ecx=&toCStr
  POPP %ebx   # ebx=&nameCStr ecx=&toStr stack: ( )
  PUSHP %edx  # stack: ( &newNode )
  call cStrMove
  // Insert new node into dictionary
  GET_P0 %edx # edx=&newNode
  DICT_META_GET_ADDR %edx, nameCStr, %ecx # %ecx=&nameCStr
  call dictCStrBucketAddr # edi=&root
  movl %edi, %eax # eax=&root
  GET_P0 %ebx   # ebx=&newNode stack: ( )
  call sllPush
  POPP %eax # eax=&node=&meta
  ret

metaSllFindName: # %eax:&node %ecx:&cStr ( -- ) %eax:&node
  // Find the name in the meta dictionary given the first node.
  PUSHP2 %eax, %ecx # ( &node, &cStr )
metaSllFindName_loop:
  JRZ %eax, metaSllFindName_dne
  addl $DICT_FIELD_nameCStr, %eax # eax=&nameCStr
  call cStrEqNocase
  JMP_E $TRUE, %eax, metaSllFindName_found
  GET_P0 %eax
  movl (%eax), %eax # eax=eax.&next
  SET_P 0, %eax # store new &node in pstack
  GET_P 1, %ecx # ecx:&cStr
  jmp metaSllFindName_loop
metaSllFindName_found:
  GET_P0 %eax
metaSllFindName_dne:
  DROPP 2
  ret

dictFindName: # %ecx:&cStr ( -- ) &eax:&meta (aka &node)
  // eax=0 if no node found.
  PUSHP %ecx
  # First try to find in localtime dict
  movl $dictLTimeRoot, %eax
  call metaSllFindName
  JRNZ %eax, foundLTime
  # Then look for in Runtime/Compiletime dicts
  GET_P0 %ecx
  call dictCStrBucketAddr
  JRZ %edi, return
  movl (%edi), %eax # eax=&firstNode
  POPP %ecx
  jmp metaSllFindName
foundLTime:
  DROPP 1
  ret

######
# Dictionary tests
.macro TEST_HASH assert, aCStr, bCStr
  # asserts that a compares to b
  MOVB_D \aCStr
  movl $\aCStr+1, %ecx
  call pjwHashNoCaseEax
  PUSHP %edi
  MOVB_D \bCStr
  movl $\bCStr+1, %ecx
  call pjwHashNoCaseEax
  POPP %ebx
  movl %edi, %eax
  call \assert
.endm
hashTest_cStr1: .byte 1
  .ascii "a"
hashTest_cStr2: .byte 2
  .ascii "aa"
hashTest_cStr3: .byte 5
  .ascii "zzzzz"
hashTest_cStr4: .byte 5
  .ascii "zzazz"
TESTCASE pjwHash
  // This isn't a very good test, but makes sure there is some spread in the
  // values.
  TEST_HASH assertEaxEbxEq, hashTest_cStr1, hashTest_cStr1
  TEST_HASH assertEaxEbxEq, hashTest_cStr2, hashTest_cStr2
  TEST_HASH assertEaxEbxEq, hashTest_cStr4, hashTest_cStr4
  TEST_HASH assertEaxEbxNe, hashTest_cStr1, hashTest_cStr2
  TEST_HASH assertEaxEbxNe, hashTest_cStr1, hashTest_cStr3
  TEST_HASH assertEaxEbxNe, hashTest_cStr3, hashTest_cStr4
  ret

.macro DICT_PUSH globalAddr, name
  SET_ABC \globalAddr, $0xDeadBeef, \name
  call dictPush
.endm

.macro TEST_GET_DICT_TYPE_GLOBAL_ADDR typeVal, type
  movl \typeVal, %eax
  call getDictTypeGlobalAddr
  ASSERT_REG_EQ %eax, $dict\type
.endm
.macro TEST_DICT_GLOBAL_GET typeVal, field, expectedAddr, expectedValue
  movl \typeVal, %eax
  call getDictTypeGlobalAddr
  DICT_GLOBAL_GET_ADDR \field, %eax
  ASSERT_REG_EQ %eax, \expectedAddr
  ASSERT_REG_EQ (%eax), \expectedValue
.endm
TESTCASE dictMacros
.set TEST_HEAP, heapMin
.equ TEST_DICT_CODE_MIN_RTIME, TEST_HEAP
.set TEST_HEAP, TEST_DICT_CODE_MIN_RTIME+DEFAULT_DICT_SECTION_SIZE
.equ TEST_DICT_CODE_MAX_RTIME, TEST_HEAP
  TEST_GET_DICT_TYPE_GLOBAL_ADDR $DICT_TYPE_RTIME, RTime
  TEST_DICT_GLOBAL_GET $DICT_TYPE_RTIME, STRUCT, $dictRTime, $heapMin
  TEST_DICT_GLOBAL_GET $DICT_TYPE_RTIME, CODE_MIN, $dictRTime, $heapMin
  TEST_DICT_GLOBAL_GET $DICT_TYPE_RTIME, CODE_MIN, $dictCodeMinRTime, $heapMin
  TEST_DICT_GLOBAL_GET $DICT_TYPE_RTIME, CODE_HERE, $dictCodeHereRTime, $heapMin
  TEST_DICT_GLOBAL_GET $DICT_TYPE_RTIME, CODE_MAX, $dictCodeMaxRTime, $TEST_DICT_CODE_MAX_RTIME
  # TODO: test others
  TEST_GET_DICT_TYPE_GLOBAL_ADDR $DICT_TYPE_CTIME, CTime
  # TODO: test compiletime
  ret

test_dictWord1CStr: .byte 9
  .ascii "TestWord1"
test_dictNameDneCStr: .byte 3
  .ascii "DNE"
TESTCASE dictPush
  movl dictMetaMinRTime, %eax
  PUSHP %eax # cache where word will be stored
  movl dictMetaHereRTime, %ebx
  DICT_PUSH $dictRTime, $test_dictWord1CStr
  GET_P0 %ebx
  call assertEaxEbxEq  # Got put at the start of meta
  movl %eax, %edi
  ASSERT_REG_EQ (%edi),   $0  # nextAddr=0
  ASSERT_REG_EQ 8(%edi),  $0  # info0=0
  addl $0xC, %edi # edi=nameCStr
  ASSERT_CSTR_NOCASE_EQ $test_dictWord1CStr, %edi
  movl $test_dictWord1CStr, %ecx
  call dictFindName
  POPP %ebx
  call assertEaxEbxEq
  movl $test_dictNameDneCStr, %ecx
  call dictFindName
  ASSERT_REG_EQ %eax, $0
  ret

// Note: execution space looks like:
// &meta | &code (dereferenced xt) | ...
//
// &meta is the meta-token and allows fast lookup from xt -> &meta -> INFO.
// For pure-assembly, &code points to "..." and "..." is the code to execute.
// For forth words, &code points to an assembly location called "doWordNext"
// which as you might expect executes words (aka xts)
//
// note: this is ONLY for new RTime or CTime words (not LTime words, which can
// be added during compilation of RTime or CTime).
dictNew: # %eax:dictType %ecx:&nameCStr ( -- ) %eax:&meta %ebx:xt
  movl %eax, compilingType # keep track of type of word being compiled.
  call getDictTypeGlobalAddr
  PUSHP %eax # stack: ( &dictGlobal )
  DICT_GLOBAL_GET_ADDR CODE_HERE, %eax
  movl (%eax), %ebx # ebx=xt-4
  PUSHP %ebx # Stack: ( xt-4 )
  addl $8, %ebx
  movl %ebx, (%eax) # update codeHere to new location
  call dictPush
  POPP %ebx # ebx=xt-4
  movl %eax, (%ebx) # set address left of xt to be backreference to &meta
  addl $4, %ebx
  DICT_META_SET %eax, xt, %ebx, %edx # set xt in metadata
  ret

################################################################################
#### Compiling Forth Words                                                  ####
################################################################################

compile: # %ebx:value ( -- )
  # compile the value into the current code space.
  movl compilingType, %eax
  call getDictTypeGlobalAddr
  DICT_GLOBAL_GET_ADDR CODE_HERE, %eax  # eax=&HERE
  movl (%eax), %ecx  # ecx=HERE
  movl %ebx, (%ecx)  # set value at HERE
  addl $CELL, (%eax) # increment HERE
  ret

.macro XT_TO_MT xtReg, reg
  movl -4(\xtReg), \reg
.endm

PANIC_LANDING INVALID_XT
compileXt: # %ebx:xt ( -- )
  // Compile an xt, checking that it's type is allowed
  JMP_B dictCodeMinRTime, %ebx, panicNow_INVALID_XT
  JMP_B dictCodeMinCTime, %ebx, compileXt_valid
  JMP_A dictCodeMaxCTime, %ebx, panicNow_INVALID_XT
compileXt_valid:
  jmp compile

################################################################################
#### Executing Forth Words                                                  ####
################################################################################
.macro NEXT
  // NEXT simply loads the next instruction to execute while also, pointing to
  // the one after that. It then runs the loaded instruction.
  //
  // Note the double indirection. Annother name for an xt may be &&code. It is
  // a double pointer to executable code.
  lodsl # same as: movl (%esi), %eax; addl $4, %esi
  jmp *(%eax)
.endm

word_next: NEXT  # a place to jmp to like `return`

doWord:
  // The executable address that most forth words point to.
  // 1. Push the next xt from the _calling word_ (esi) onto the return stack
  pushl %esi
  // 2. Add CELL to our own xt (stored at eax from NEXT) and make esi point to
  //    the next cell
  addl $CELL, %eax
  movl %eax, %esi
  NEXT

callXt: # %eax:xt ( * -- * )
  // Executes an xt, returning control to this function when complete.  This
  // can be used to execute forth words from within assembly.
  pushl %esi  # store current esi so it can be restored
  movl $callXt_exit, %esi # set the "currently executing" to own control flow
  jmp *(%eax)
.align 4
# The double indirection is there because of how DOCOL and EXIT work together.
callXt_exit: .int callXt_xt # kind of a "fake exit"
callXt_xt: .int callXt_cont # kind of a "fake xt"
callXt_cont:
  popl %esi # restore esi
  ret

execOrComp: # %eax:xt ( -- )
  // Execute or compile the xt based on compileState and xt.meta.IMM
  JMP_E $0, compileState, callXt # if compileState=0 just run it
  XT_TO_MT %eax, %ebx
  addl $DICT_FIELD_info0, %ebx
  movl (%ebx), %ebx  # ebx=info0
  JMP_TESTZ $INFO0_IMM, %ebx, execOrCompile_compile
  jmp callXt
execOrCompile_compile:
  movl %eax, %ebx
  jmp compileXt

.macro CALL_XT xt
  movl \xt, %eax
  call callXt
.endm

##############################
# Main Entrypoint and test suite

testSuite:
  call assertClean

  call testcase_pureAsm
  call testcase_pstack

  call testcase_bcmp
  call testcase_beqNocase
  call testcase_bmove
  call testcase_align4
  call testcase_sll
  call testcase_asciiToHex

  call testcase_findWhitespace
  call testcase_fillSourceCStr
  call testcase_flush_sourceCStr_to_wordCStr
  call testcase_word

  call testcase_pjwHash
  call testcase_dictMacros
  call testcase_dictPush

  jmp assertClean

setupHeap:
  // Sets up the heap, including the dictionary and other data structures.
  movl $heapMin, heap
  jmp dictSetup

setupStack:
  movl $rstackMinRoot, rstackMin
  movl $rstackMaxRoot, rstackMax
  movl $pstackMinRoot, pstackMin
  movl $pstackMaxRoot, pstackMax
  movl rstackMax, %esp  # clear rstack
  movl pstackMax, %ebp  # clear pstack
  jmp afterSetupStack # Note: cannot `ret` as we've modified rstack

mainStartNtStr: .ascii "Starting Triforth...\n\0"
.globl _start
_start:
  movl %esp, initialEsp # arguments and environment variables stored here
  jmp setupStack
afterSetupStack:
  call setupHeap
  PNT_NTSTR $mainStartNtStr
  call testSuite
  EXIT $0

.section .data
.set PREV_WORD_MT, 0

################################################################################
#### Pure Assembly Forth Words                                              ####
################################################################################
// Note: we must define these in the .data section because we must mutate the
// metaAddr_\name memory to point to the actual metadata once the word is added
// to the dictionary (and you cannot mutate memory in the .text section).
//
// A pure assembly forth word will have the following structure:
// &meta | &code -> | nameNtStr | code
//
// - &meta: WILL point to the "metadata" of the word in the dictionary. For now,
//   (since we will add the words later) it will point to the previously
//   defined word
// - &code: contains pointer to the code section. A pointer to this is called
//   the "xt"
// - nameNtName is the words null-terminated ascii name. This, along with the
//   singly linked list formed by the &meta will be used to populate the
//   dictionary before execution of forth source code.
// - code: the code to execute for this word.

.macro ASM_WORD_NAME name, asmName
  .align 4
  metaAddr_\asmName :        .int PREV_WORD_MT
  xt_\asmName:          .int word_\asmName
  nameNtStr_\asmName :     .ascii "\name\0"
  word_\asmName :
  .set PREV_WORD_MT,    metaAddr_\asmName
.endm
.macro ASM_WORD name
  ASM_WORD_NAME "\name", "\name"
.endm
.macro NEXT_ASM_WORD_NAME name, asmName
  NEXT
  ASM_WORD_NAME "\name", "\asmName"
.endm
.macro NEXT_ASM_WORD name
  NEXT
  ASM_WORD "\name"
.endm

ASM_WORD "exit"
  popl %esi

// ########
// # Parameter Stack Operations
NEXT_ASM_WORD "swap" # ( c0 c1 -- c1 c0 )
  POPP2 %eax, %ebx
  UNCK_PUSHP2 %ebx, %eax
NEXT_ASM_WORD "dup" # ( c0 -- c0 c0 )
  POPP %eax
  PUSHP2 %eax, %eax
NEXT_ASM_WORD "2dup" # ( c0 c1 -- c0 c1 c0 c1 )
  POPP2 %eax, %ebx
  UNCK_PUSHP2 %eax, %ebx
  PUSHP2 %eax, %ebx
NEXT_ASM_WORD "3dup" # ( c0 c1 c2-- c0 c1 c2 c0 c1 c2 )
  POPP3 %eax, %ebx, %ecx
  UNCK_PUSHP3 %eax, %ebx, %ecx
  PUSHP3 %eax, %ebx, %ecx
NEXT_ASM_WORD "lrot" # ( c0 c1 c2 -- c1 c2 c0 )
  POPP3 %eax, %ebx, %ecx
  UNCK_PUSHP3 %ebx, %ecx, %eax
NEXT_ASM_WORD "rrot" # ( c0 c1 c2 -- c2 c0 c1 )
  POPP3 %eax, %ebx, %ecx
  UNCK_PUSHP3 %ecx, %eax, %ebx
NEXT_ASM_WORD "over" # ( c0 c1 -- c0 c1 c0 )
  POPP2 %eax, %ebx
  PUSHP3 %eax, %ebx, %eax
NEXT_ASM_WORD "nip" # ( c0 c1 -- c1 )
  POPP2 %eax, %ebx
  UNCK_PUSHP %ebx
NEXT_ASM_WORD "tuck" # ( c0 c1-- c0 c1 c0 )
  POPP2 %eax, %ebx
  PUSHP3 %eax, %ebx, %eax
NEXT_ASM_WORD "pick" # ( index ... -- c ... ) \ Get cell at index
  POPP %eax
  movl %eax, %ebx
  CHECK_PSTACK_INDEX %ebx
  movl (%ebp,%eax,CELL), %eax
  PUSHP %eax
NEXT_ASM_WORD "replace" # ( index c ... -- .... ) \ replace cell at index with c
  POPP2 %eax, %ebx
  movl %eax, %ecx
  CHECK_PSTACK_INDEX %ecx
  movl %ebx, (%ebp,%eax,CELL)
NEXT_ASM_WORD_NAME "?dup", maybeDup # ( c -- c c? ) \ dup top of stack if non-zero
  POPP %eax
  PUSHP %eax
  JRZ %eax, word_next
  PUSHP %eax
.macro NEXT_ASM_WORD_MACRO_N name, do, n
  NEXT_ASM_WORD "\name"
  \do \n
.endm
NEXT_ASM_WORD_MACRO_N "drop", DROPP, 1    # ( c0 -- )
NEXT_ASM_WORD_MACRO_N "2drop", DROPP, 2   # ( c0 c1 -- )
NEXT_ASM_WORD_MACRO_N "3drop", DROPP, 3   # ( c0 c1 c2 -- )
NEXT_ASM_WORD_MACRO_N "4drop", DROPP, 4   # ( c0 c1 c2 c3 -- )

// ########
// # Return Stack Operations
NEXT_ASM_WORD "rPush" # p( c0 -- ) r( -- c0 )
  POPP %eax
  PUSHR %eax
NEXT_ASM_WORD "rPop" # p( -- c0 ) r( c0 -- )
  POPR %eax
  PUSHP %eax
NEXT_ASM_WORD "rPushN" # p( c:N c0...cN-1 -- ) r( -- c0...cN-1 )
  TODO
NEXT_ASM_WORD "rPopN" # p( c:N -- c0...cN-1 ) r( c0...N-1 -- )
  TODO

// ########
// # Address Operations
NEXT_ASM_WORD_NAME "@", fetch  # ( &c -- c ) \ fetch cell at address
  POPP %eax
  movl (%eax), %eax
  PUSHP %eax
NEXT_ASM_WORD_NAME "f@", bFetch  # ( &b -- b ) \ fetch byte at address
  POPP %eax
  ZERO %ebx
  movb (%eax), %bl
  PUSHP %ebx
NEXT_ASM_WORD_NAME "t@", tFetch  # ( &t -- t ) \ fetch tyte at address
  POPP %eax
  ZERO %ebx
  movw (%eax), %bx
  PUSHP %ebx
NEXT_ASM_WORD_NAME "!", store  # ( c &c -- ) \ store value to address
  POPP2 %eax, %ebx
  movl %eax, (%ebx)
NEXT_ASM_WORD_NAME "!+", incAddr  # ( &c -- ) \ increment value at addr
  POPP %eax
  addl $1, (%eax)
NEXT_ASM_WORD_NAME "!-", decAddr  # ( &c -- ) \ derement value at addr
  POPP %eax
  subl $1, (%eax)
NEXT_ASM_WORD_NAME "b!", bStore  # ( b &b -- ) \ store byte at addr
  POPP2 %eax, %ebx
  movb %al, (%ebx)
NEXT_ASM_WORD_NAME "t!", tStore  # ( t &t -- ) \ store tyte at addr
  POPP2 %eax, %ebx
  movw %ax, (%ebx)
NEXT_ASM_WORD_NAME "&dstack" getDstackAddr # ( -- &dstack ) \ get the &dstack
  PUSHP %ebp
NEXT_ASM_WORD_NAME "!dstack", setDstackAddr # ( addr -- ??? ) \ set the &dstack
  POPP %ebp
NEXT_ASM_WORD_NAME "&rstack", getRstackAddr # ( -- &rstack ) \ get the &rstack
  PUSHP %esp
NEXT_ASM_WORD_NAME "!rstack", setRstackAddr # ( addr -- ) \ set the &rstack
  POPP %esp

// ########
// # Arithmetic Operations
.macro NEXT_SINGLE_EAX_OP name, asmName, op # ( c0 -- [OP c0] )
  NEXT_ASM_WORD_NAME "\name", "\asmName"
    POPP %eax
    \op
    PUSHP %eax
.endm
NEXT_SINGLE_EAX_OP "+1", "inc", "inc %eax"          # ( c -- c+1 )
NEXT_SINGLE_EAX_OP "+2", "inc2", "addl $2, %eax"    # ( c -- c+2 )
NEXT_SINGLE_EAX_OP "+4", "inc4", "addl $4, %eax"    # ( c -- c+4 )
NEXT_SINGLE_EAX_OP "-", "dec", "dec %eax"           # ( c -- c-1 )
NEXT_SINGLE_EAX_OP "-2", "dec2", "subl $2, %eax"    # ( c -- c-2 )
NEXT_SINGLE_EAX_OP "-4", "dec4", "subl $4, %eax"    # ( c -- c-4 )
NEXT_SINGLE_EAX_OP "2/", "div2", "shr $1, %eax"     # ( c -- c/2 )
NEXT_SINGLE_EAX_OP "4/", "div4", "shr $2, %eax"     # ( c -- c/4 )
NEXT_SINGLE_EAX_OP "8/", "div8", "shr $3, %eax"     # ( c -- c/8 )
NEXT_SINGLE_EAX_OP "x10/", "divx10", "shr $4, %eax" # ( c -- c/0x10 )
NEXT_SINGLE_EAX_OP "2*", "mul2", "shl $1, %eax"     # ( c -- c*2 )
NEXT_SINGLE_EAX_OP "4*", "mul4", "shl $2, %eax"     # ( c -- c*4 )
NEXT_SINGLE_EAX_OP "8*", "mul8", "shl $3, %eax"     # ( c -- c*8 )
NEXT_SINGLE_EAX_OP "x10*", "mulx10", "shl $4, %eax" # ( c -- c*0x10 )
NEXT_SINGLE_EAX_OP "inv", "inv", "not %eax"         # ( c -- ~c )
NEXT_SINGLE_EAX_OP "align4", "align4", "ALIGN4 %eax" # ( c -- c )
NEXT_ASM_WORD "not" # ( c -- bool) \ logical not, if 0 then TRUE else FALSE
  POPP %eax
  JRZ %eax, not_wasFalse
  PUSHP $FALSE
  NEXT
not_wasFalse:
  PUSHP $TRUE
NEXT_ASM_WORD_NAME "=0", "eq0" # ( c -- bool ) same as NOT
  jmp word_not
NEXT_ASM_WORD "bool" # ( c -- bool )
  POPP %eax
  JRZ %eax, bool_false
  PUSHP $TRUE
  NEXT
bool_false:
  PUSHP $FALSE

.macro NEXT_ARITHMETIC_OP name, asmName, op # ( c0 c1 -- [c0 OP c1] )
  NEXT_ASM_WORD_NAME "\name", "\asmName"
    addl $CELL*2, %ebp    # subtract 2 cells to check stack size
    cmpl pstackMax, %ebp
    ja invalidPopp2
    subl $CELL, %ebp       # return stack to item that will remain
    movl -CELL(%ebp), %eax # move left operand
    \op %eax, (%ebp)
.endm
NEXT_ARITHMETIC_OP "+", "add", addl                 # ( c c -- [c+c] )
NEXT_ARITHMETIC_OP "-", "sub", subl                 # ( c c -- [c-c] )
NEXT_ARITHMETIC_OP "and", "and", andl               # ( c c -- [c&c] )
NEXT_ARITHMETIC_OP "or", "or", orl                  # ( c c -- [c|c] )
NEXT_ARITHMETIC_OP "xor", "xor", xorl               # ( c c -- [c^c] )

// #######
// # nShL and nShR
// The assembly shift-left and shift-right functions only take a CONSTANT.
// Therefore we have to create a jump table so that we can shift by 0-31 bits
// in forth
.macro INT_NSH direction, N
  .int Nsh\N\direction
.endm
.macro LOC_NSH direction, N
Nsh\N\direction :
  sh\direction $0x\N , %eax
  NEXT
.endm
.macro NSH_BLOCK4 cmd, direction, n0, n1, n2, n3, n4, n5, n6, n7
    \cmd \direction, \n0
    \cmd \direction, \n1
    \cmd \direction, \n2
    \cmd \direction, \n3
.endm
.macro NSH_BLOCK8 cmd, direction, n0, n1, n2, n3, n4, n5, n6, n7
    NSH_BLOCK4 \cmd, \direction, \n0, \n1, \n2, \n3
    NSH_BLOCK4 \cmd, \direction, \n4, \n5, \n6, \n7
.endm
.macro DEFINE_NSH direction
  Nsh\direction : # %eax:value %ebx:numShift ( -- ) %eax:shiftedValue
    JMP_A $0x100, %ebx, Nsh_ret0_\direction
    jmp *Nsh_jt_\direction(,%ebx,4)
  .align 4
  Nsh_jt_\direction :
    NSH_BLOCK8 INT_NSH, \direction, 00, 01, 02, 03, 04, 05, 06, 07
    NSH_BLOCK8 INT_NSH, \direction, 08, 09, 0A, 0B, 0C, 0D, 0E, 0F
    NSH_BLOCK8 INT_NSH, \direction, 10, 11, 12, 13, 14, 15, 16, 17
    NSH_BLOCK8 INT_NSH, \direction, 18, 19, 1A, 1B, 1C, 1D, 1E, 1F

    NSH_BLOCK8 LOC_NSH, \direction, 00, 01, 02, 03, 04, 05, 06, 07
    NSH_BLOCK8 LOC_NSH, \direction, 08, 09, 0A, 0B, 0C, 0D, 0E, 0F
    NSH_BLOCK8 LOC_NSH, \direction, 10, 11, 12, 13, 14, 15, 16, 17
    NSH_BLOCK8 LOC_NSH, \direction, 18, 19, 1A, 1B, 1C, 1D, 1E, 1F
  Nsh_ret0_\direction :
    PUSHP $0
.endm

NEXT_ASM_WORD "shl" # ( n c -- c<<n )
  DEFINE_NSH l 
NEXT_ASM_WORD "shr" # ( n c -- c>>n )
  DEFINE_NSH r
NEXT_ASM_WORD_NAME "/mod" divMod # ( dividend divisor -- quotient remainder )
  ZERO %edx
  POPP2 %eax, %ebx
  divl %ebx # %edx:%eax / %ebx => %eax=quotient %edx=remainder
  PUSHP2 %eax, %edx

// ########
// # Comparison Operations
.macro NEXT_CMP_OP name, asmName, jmp_cmp # ( c0 c1 -- [jmp_cmp c1, c0, true] )
  POPP2 %eax, %ebx
  \jmp_cmp %ebx, %eax, 1f
  PUSHP $FALSE
  NEXT
1:
  PUSHP $TRUE
  NEXT
.endm

NEXT_CMP_OP "<", "lt", "JMP_L"      # ( c0 c1 -- [c0 < c1] )
NEXT_CMP_OP "<=", "lte", "JMP_LE"   # ( c0 c1 -- [c0 <= c1] )
NEXT_CMP_OP "=", "eq", "JMP_E"      # ( c0 c1 -- [c0 = c1] )
NEXT_CMP_OP ">=", "gte", "JMP_GE"   # ( c0 c1 -- [c0 >= c1] )
NEXT_CMP_OP ">", "gt", "JMP_G"      # ( c0 c1 -- [c0 > c1] )

NEXT_CMP_OP "u<", "ult", "JMP_B"      # ( c0 c1 -- [c0 u< c1] )
NEXT_CMP_OP "u<=", "ulte", "JMP_BE"   # ( c0 c1 -- [c0 u<= c1] )
NEXT_CMP_OP "u>=", "ugte", "JMP_AE"   # ( c0 c1 -- [c0 u>= c1] )
NEXT_CMP_OP "u>", "ugt", "JMP_A"      # ( c0 c1 -- [c0 u> c1] )

// ########
// # Memory Operations
NEXT_ASM_WORD bcmp # ( aCount &a bCount &b -- comparison )
  POPP4 %ebx, %eax, %edx, %ecx
  call bcmp
  PUSHP %eax
NEXT_ASM_WORD beqNocase # ( aCount &a bCount &b -- bool )
  POPP4 %ebx, %eax, %edx, %ecx
  call beqNocase
  PUSHP %eax
NEXT_ASM_WORD bmove # ( c:count c:&dst c:&src -- )
  POPP3 %eax, %ecx, %ebx
  call bmove
NEXT_ASM_WORD bmoveRight # ( c:count c:&dst c:&src -- )
  TODO
NEXT_ASM_WORD cmove # ( c:count c:&dst c:&src -- )
  POPP3 %eax, %ecx, %ebx
  call cmove
NEXT_ASM_WORD fzero # ( count &f -- ) \ zero count fytes
  POPP2 %eax, %ebx
  call fzero

// ########
// # String Operations
NEXT_ASM_WORD countNtStr # ( &ntStr -- c &str )
  POPP %ecx
  call countntEcx
  PUSHP2 %edx, %ecx
NEXT_ASM_WORD countCStr # ( &cStr -- c &str )
  POPP %ecx
  MOVB_A (%ecx) # eax=count
  inc %ecx
  PUSHP2 %eax, %ecx
NEXT_ASM_WORD cmpCStr # ( &aCStr &bCStr -- comparison )
  POPP2 %eax, %ecx
  call cmpCStr
  PUSHP %eax
NEXT_ASM_WORD cStrEqNocase # ( &aCStr &bCStr -- comparison )
  POPP2 %eax, %ecx
  call cStrEqNocase
  PUSHP %eax
NEXT_ASM_WORD cStrMove # ( &dstCStr &srcCStr -- )
  POPP2 %ecx, %ebx
  call cStrMove

// ########
// # Number formatting
NEXT_ASM_WORD asciiToHexFyte # ( count &b -- c )
  POPP2 %edx, %ecx
  call asciiToHex
  PUSHP %eax
NEXT_ASM_WORD asciiToBinaryFyte # ( count &b -- c )
  POPP2 %edx, %ecx
  call asciiToBinary
  PUSHP %eax

################################################################################
#### Forth Variables and Constants                                          ####
################################################################################

.macro NEXT_ASM_CONSTANT name # define a constant
  NEXT_ASM_WORD "\name"
  PUSHP $\name
.endm
.macro NEXT_ASM_CONSTANT_ADDR name # define an address
  NEXT_ASM_WORD_NAME "&\name", "addr\name"
  PUSHP $\name
.endm

NEXT_ASM_CONSTANT TRUE
NEXT_ASM_CONSTANT FALSE
NEXT_ASM_CONSTANT CELL
NEXT_ASM_CONSTANT SYS_OPEN
NEXT_ASM_CONSTANT SYS_WRITE
NEXT_ASM_CONSTANT SYS_READ
NEXT_ASM_CONSTANT SYS_CLOSE
NEXT_ASM_CONSTANT SYS_EXIT
NEXT_ASM_CONSTANT O_RDONLY
NEXT_ASM_CONSTANT O_WRONLY
NEXT_ASM_CONSTANT O_RDWR
NEXT_ASM_CONSTANT WORD_CNT_MAX

# Compiler Intrinsics
NEXT_ASM_CONSTANT_ADDR compileState

# Stack Address Constants
NEXT_ASM_CONSTANT_ADDR rstackMin
NEXT_ASM_CONSTANT_ADDR rstackMax
NEXT_ASM_CONSTANT_ADDR pstackMin
NEXT_ASM_CONSTANT_ADDR pstackMax
NEXT_ASM_CONSTANT_ADDR initialEsp

# Word/Source Address Constants
NEXT_ASM_CONSTANT_ADDR pntFd
NEXT_ASM_CONSTANT_ADDR compilingType
NEXT_ASM_CONSTANT_ADDR sourceFd
NEXT_ASM_CONSTANT_ADDR sourceEof
NEXT_ASM_CONSTANT_ADDR sourceCStr
NEXT_ASM_CONSTANT_ADDR sourceStr
NEXT_ASM_CONSTANT_ADDR wordCStr
NEXT_ASM_CONSTANT_ADDR wordStr

# Error/Panic Address Constants
NEXT_ASM_CONSTANT_ADDR errno
NEXT_ASM_CONSTANT_ADDR panic
NEXT_ASM_CONSTANT_ADDR panicErr
NEXT_ASM_CONSTANT_ADDR panicNtStr

# Dictionary Constants
NEXT_ASM_CONSTANT GLOBAL_DICT_OFFSET_STRUCT
NEXT_ASM_CONSTANT GLOBAL_DICT_OFFSET_CODE_MIN
NEXT_ASM_CONSTANT GLOBAL_DICT_OFFSET_CODE_HERE
NEXT_ASM_CONSTANT GLOBAL_DICT_OFFSET_CODE_MAX
NEXT_ASM_CONSTANT GLOBAL_DICT_OFFSET_META_MIN
NEXT_ASM_CONSTANT GLOBAL_DICT_OFFSET_META_HERE
NEXT_ASM_CONSTANT GLOBAL_DICT_OFFSET_META_MAX
NEXT_ASM_CONSTANT DEFAULT_DICT_BUCKETS_CNT

NEXT_ASM_CONSTANT_ADDR dictBucketsAddr
NEXT_ASM_CONSTANT_ADDR dictBucketsAndValue 
NEXT_ASM_CONSTANT_ADDR dictLTimeRoot
NEXT_ASM_CONSTANT_ADDR dictLTimeMin
NEXT_ASM_CONSTANT_ADDR dictLTimeHere
NEXT_ASM_CONSTANT_ADDR dictLTimeMax

################################################################################
#### Execution Loop                                                         ####
################################################################################
# The execution loop starts reading from sourceFd and simply executes until it is
# complete.
NEXT_ASM_WORD executionLoop

heap: .int 0 # see setupMem
.align 0x100
heapMin:                     .space 0x200000  # 2MiB TODO: do dynamically from runtime
heapDefaultMax:

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#define u32 uint32_t
#define u64 uint64_t
#define xt u32

// Parameter stack
u32* pstackMin;
u32* pstackMax;
u32* psp;

// Return stack
u32* rstackMin;
u32* rstackMax;
u32* rsp;


void pntStack() {
  u32* ptr = pstackMax;
  printf("DSTACK: ");
  while (ptr != psp) {
    ptr -= sizeof(u32);
    printf("%X ", *ptr);
  }
  printf("\n");
}

void defaultPanic() {
  printf("Panicing\n");
  pntStack();
  exit(100);
}

void (*panic)() = &defaultPanic;


// ##################
// # Parameter stack manipulation
void pushd(u32 value) {
  if (psp <= pstackMin) {
    printf("pstack overflow\n");
    exit(1);
  }
  psp -= sizeof(u32);
  *psp = value;
}

u32 popd() {
  if (psp >= pstackMax) {
    printf("pstack underflow\n");
    exit(1);
  }
  u32 out = *psp;
  psp += sizeof(u32);
  return out;
}

// ##################
// # Return stack manipulation
void pushr(u32 value) {
  if (rsp <= rstackMin) {
    printf("rstack overflow\n");
    exit(1);
  }
  rsp -= sizeof(u32);
  *rsp = value;
}

u32 popr() {
  if (rsp >= rstackMax) {
    printf("rstack underflow\n");
    exit(1);
  }
  u32 out = *rsp;
  rsp += sizeof(u32);
  return out;
}

// ##################
// # Execution. This must contain ALL builtin forth words.

// Where forth code is stored.
xt* code;
u32 codeSize;

// The c type of a forth "word". It has no inputs/outputs
// becuase it must use the stack
u32 codei;  // execution token instruction pointer

typedef enum {
  EXIT = 0xFFFF0000,
  MUL,
} BuiltinWord;

// Execute a forth word
void execute() {
  while (rsp < rstackMax) {
    xt x = code[codei];
    switch (x) {
      case EXIT:
        codei = popr();
        continue;
      case MUL:
        pushd(popd() * popd());
        break;
    }
    codei += 1;
  }
}

void testExecute() {
  code[0] = MUL;
  code[1] = EXIT;
  codei = 0;
  pushd(21);
  pushd(2);
  pushr(0xFFFFFFFF);
  execute();
  assert(popd() == 42);
}

// ##################
// # Main Initialization

#define PSTACK_SIZE (0x400)
#define RSTACK_SIZE (0x1000)
#define CODE_START_SIZE (0x10000)

int main() {
  pstackMin = malloc(PSTACK_SIZE + RSTACK_SIZE);
  if (!pstackMin) {
    printf("Could not reserve stacks\n");
    exit(1);
  }
  pstackMax = pstackMin + PSTACK_SIZE;
  psp = pstackMax;

  rstackMin = pstackMax;
  rstackMax = rstackMin + RSTACK_SIZE;
  rsp = rstackMax;

  code = malloc(CODE_START_SIZE);
  codeSize = CODE_START_SIZE;
  if (!code) {
    printf("Could not reserve code\n");
    exit(1);
  }

  testExecute();

  return 0;
}

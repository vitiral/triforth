#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define s uint16_t // single
#define sSize 2    // size in bytes
#define d uint32_t // double
#define dSize 4    // size in bytes

// Parameter stack
s* pstackMin;
s* pstackMax;
s* psp;

// Return stack
s* rstackMin;
s* rstackMax;
s* rsp;

// Base memory of 16bit pointers
s* base;
s* vip;  // virtual instruction pointer

void pntStack() {
  s* ptr = pstackMax;
  printf("DSTACK: ");
  while (ptr != psp) {
    ptr -= sSize;
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
void pushd(s value) {
  if (psp <= pstackMin) {
    printf("pstack overflow\n");
    exit(1);
  }
  psp -= sSize;
  *psp = value;
}

s popd() {
  if (psp >= pstackMax) {
    printf("pstack underflow\n");
    exit(1);
  }
  s out = *psp;
  psp += sSize;
  return out;
}

// ##################
// # Return stack manipulation
void pushr(s value) {
  if (rsp <= rstackMin) {
    printf("rstack overflow\n");
    exit(1);
  }
  rsp -= sSize;
  *rsp = value;
}

s popr() {
  if (rsp >= rstackMax) {
    printf("rstack underflow\n");
    exit(1);
  }
  s out = *rsp;
  rsp += sSize;
  return out;
}

// ##################
// # Execution. This must contain ALL builtin forth words.

typedef enum {
  EXIT,
} Word;

void next() {
  Word w = (Word) *vip;
  switch w {
    case EXIT:
      printf("EXIT\n");
  }
}

// ##################
// # Main Initialization

#define PSTACK_SIZE (0x400)
#define RSTACK_SIZE (0x1000)

int main() {
  base = malloc(0x10000); // 0i64 kilobytes
  if (!base) {
    printf("Could not reserve base 16bit memory\n");
    exit(1);
  }

  pstackMin = base;
  pstackMax = pstackMin + PSTACK_SIZE;
  psp = pstackMax;

  rstackMin = pstackMax;
  rstackMax = rstackMin + RSTACK_SIZE;
  rsp = rstackMax;

  pushd(0x42);
  pushd(0x43);
  (*panic)();

  return 0;
}

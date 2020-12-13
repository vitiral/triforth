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
s pstackSize = 0;
s* psp;

// Return stack
s* rstack;
s rstackSize = 0;
s* rsp;

void pntStack() {
  s* ptr = pstackMin + pstackSize;
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

void (*panic)() = defaultPanic;


// ##################
// # Data stack manipulation
void pushd(s value) {
  psp -= sSize;
  *psp = value;
}

s popd() {
  s out = *psp;
  psp += sSize;
  return out;
}

void pushr(s value) {
  rsp -= sSize;
  *rsp = value;
}

s popr() {
  s out = *rsp;
  rsp += sSize;
  return out;
}


#define STACK_SIZE (0x400)

int main() {
  pstackMin = malloc(STACK_SIZE);
  pstackSize = STACK_SIZE;
  psp = pstackMin + STACK_SIZE;

  rstack = malloc(STACK_SIZE);
  rstackSize = STACK_SIZE;
  rsp = rstack + STACK_SIZE;

  if (!psp || !rsp) {
    printf("Could not reserve pstack or rstack\n");
    exit(1);
  }

  pushd(0x42);
  pushd(0x43);
  panic();

  return 0;
}

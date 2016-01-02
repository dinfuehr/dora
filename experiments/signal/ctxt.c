#include <stdio.h>
#include <stdint.h>
#include <signal.h>
#include <unistd.h>
#include <sys/mman.h>
#include <string.h>
#include <ucontext.h>

#define REG_RIP 16

typedef int (*ftype)();

ftype alloc_code(const char *code, size_t len) {
  long pagesize;

  if ((pagesize = sysconf(_SC_PAGESIZE)) == -1) {
    perror("sysconf(_SC_PAGESIZE) failed");
  }

  void *ptr = mmap(NULL, pagesize, PROT_READ | PROT_WRITE | PROT_EXEC,
    MAP_ANON | MAP_PRIVATE, -1, 0);

  memcpy(ptr, code, len);
  return ptr;
}

void handler(int signo, siginfo_t *info, void *context) {
  ucontext_t *ucontext = context;
  mcontext_t *mcontext = &ucontext->uc_mcontext;

  printf("signal %d!\n", signo);
  uint8_t *xpc = (uint8_t*) mcontext->gregs[REG_RIP];

  printf("exception program counter = %p: ", xpc);

  for(int i=0; i<8; i++) {
    printf("%x, ", xpc[i]);
  }

  printf("\n");

  // mov eax, 4
  // ret
  unsigned char code[] = { 0xb8, 4, 0, 0, 0, 0xc3 };
  ftype fct = alloc_code(code, sizeof(code));

  mcontext->gregs[REG_RIP] = (greg_t) fct;
}

int main() {
  struct sigaction sa;

  sa.sa_sigaction = handler;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = SA_SIGINFO;

  if (sigaction(SIGSEGV, &sa, NULL) == -1) {
    perror("sigaction failed");
  }

  // mov r10, [9]
  unsigned char code[] = { 0x4C, 0x8B, 0x14, 0x25, 9, 0, 0, 0 };
  ftype fct = alloc_code(code, sizeof(code));

  printf("address of failure code = %p\n", fct);

  int res = fct();
  printf("res = %d\n", res);

  return 0;
}

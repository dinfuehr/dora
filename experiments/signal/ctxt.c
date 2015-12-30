#include <stdio.h>
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

void free_code(void *ptr) {
  long pagesize;

  if ((pagesize = sysconf(_SC_PAGESIZE)) == -1) {
    perror("sysconf(_SC_PAGESIZE) failed");
  }

  if (munmap(ptr, pagesize) == -1) {
    perror("munmap failed");
  }
}

void handler(int signo, siginfo_t *info, void *context) {
  printf("signal %d!\n", signo);

  unsigned char code[] = { 0xb8, 1, 0, 0, 0, 0xc3 };
  ftype fct = alloc_code(code, sizeof(code));

  ucontext_t *ucontext = context;
  ucontext->uc_mcontext.gregs[REG_RIP] = (greg_t) fct;
}

int main() {
  struct sigaction sa;

  sa.sa_sigaction = handler;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = SA_SIGINFO;

  if (sigaction(SIGSEGV, &sa, NULL) == -1) {
    perror("sigaction failed");
  }

  // int res = fct();
  // printf("res = %d\n", res);
  //
  // free_code(fct);
  return 0;
}

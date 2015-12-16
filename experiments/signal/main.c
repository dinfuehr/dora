#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include <sys/mman.h>
#include <string.h>

void handler(int signo, siginfo_t *info, void *context) {
  printf("signal %d!\n", signo);
}

int main() {
  struct sigaction sa;

  sa.sa_sigaction = handler;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = SA_SIGINFO;

  if (sigaction(SIGINT, &sa, NULL) == -1) {
    perror("sigaction failed");
  }

  long pagesize;

  if ((pagesize = sysconf(_SC_PAGESIZE)) == -1) {
    perror("sysconf(_SC_PAGESIZE) failed");
  }

  printf("pagesize = %ld\n", pagesize);

  void *ptr = mmap(NULL, pagesize, PROT_READ | PROT_WRITE | PROT_EXEC,
    MAP_ANON | MAP_PRIVATE, -1, 0);

  unsigned char code[] = { 0xb8, 1, 0, 0, 0, 0xc3 };
  memcpy(ptr, code, sizeof(code));
  int (*fct)() = ptr;

  int res = fct();
  printf("res = %d\n", res);

  if (munmap(ptr, pagesize) == -1) {
    perror("munmap failed");
  }

  return 0;
}

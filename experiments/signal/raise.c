#include <stdio.h>
#include <stdint.h>
#include <signal.h>
#include <ucontext.h>

#define REG_RBP 10
#define REG_RSP 15
#define REG_RIP 16

void handler(int signo, siginfo_t *info, void *context) {
  printf("signal %d!\n", signo);

  ucontext_t *ucontext = context;
  mcontext_t *mcontext = &ucontext->uc_mcontext;

  uintptr_t rip = mcontext->gregs[REG_RIP];
  printf("rip  = %p\n", rip);

  uintptr_t rbp = mcontext->gregs[REG_RBP];
  printf("rbp = %p\n", rbp);

  uintptr_t rsp = mcontext->gregs[REG_RSP];
  printf("rsp = %p\n", rsp);
}

int main() {
  struct sigaction sa;

  sa.sa_sigaction = handler;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = SA_SIGINFO;

  if (sigaction(SIGINT, &sa, NULL) == -1) {
    perror("sigaction failed");
  }

  raise(SIGINT);

  printf("after raising and handling SIGINT\n");
  return 0;
}

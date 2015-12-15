#include <stdio.h>
#include <signal.h>

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

  return 0;
}

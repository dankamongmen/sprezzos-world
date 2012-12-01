#include <stdio.h>
int main () {
#ifdef __GLIBC__
  if (__GLIBC_MINOR__ >= 2) {
    return 0;
  }
  else {
    printf("Error: You need glibc >= 2.2 at least.\n");
    return 1;
  }
#else
  printf("Warning: You are not using glibc.\n");
  printf("This architecture is not officially supported.\n");
  printf("Consider yourself lucky if it works for you.\n");
  printf("Please do not complain if it does not.\n");
  return 0;
#endif

}

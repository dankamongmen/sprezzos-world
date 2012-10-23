#include <stdio.h>

extern int test_hash_run();
extern int test_system_packages_run();

static int (*func_run[])() ={
  test_hash_run,
  test_system_packages_run,
  NULL
};

int
main() {
  int result=0;
  int i=0;

  for(i=0; !result && func_run[i]; i++) {
    result = func_run[i]();
    }

  return result;
}

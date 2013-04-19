#include <pstreams/pstream.h>

int main() {

  std::string output;
  redi::ipstream one_two_three("echo 123");
  one_two_three >> output;
  if (output != "123") {
    return 1;
  }

  return 0;
}

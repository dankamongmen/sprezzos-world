#include <pstreams/pstream.h>
#include <iostream>

int main() {
  std::string output;
  redi::pstream sed("sed -e s/x/./g", redi::pstream::pstdin | redi::pstream::pstdout | redi::pstream::pstderr);
  sed << "xxx" << std::endl << redi::peof;
  std::getline(sed.out(), output);
  sed.close();
  if (output != "...") {
    return 1;
  }

  return 0;
}

#include <iostream>
#include <fstream>

#define BAD_MANPAGE "/usr/share/man/man3/deprecated.3.gz"

int main() {
  using namespace std;
  ifstream bad_manpage(BAD_MANPAGE, ifstream::in);
  if (!bad_manpage.fail()) {
    cerr << "Bad manpage installed: " << BAD_MANPAGE << endl;
    return 1;
  }
  return 0;
}

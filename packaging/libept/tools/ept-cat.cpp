#include <ept/apt/apt.h>
#include <ept/apt/apt.h>

int summary = 0;

int main( int argc, char **argv ) {
    using namespace ept::apt;
    using namespace std;

    Apt db;

    for (Apt::record_iterator i = db.recordBegin(); i != db.recordEnd(); ++i)
	    cout << *i << endl;
   
    return 0;
}

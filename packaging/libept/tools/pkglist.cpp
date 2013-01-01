#include <iostream>

using namespace std;

#include <ept/apt/apt.h>


int main(int argc, char* argv[])
{
	typedef ept::apt::Apt Apt;
	Apt apt;
	for (Apt::Iterator it = apt.begin(); it != apt.end(); ++it)
	{
		cout << (*it) << endl;
	}
}

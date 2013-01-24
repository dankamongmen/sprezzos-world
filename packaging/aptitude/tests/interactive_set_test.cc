// A simple test that allows you to interactively build sets.

#include <generic/util/immset.h>

#include <iostream>

int main(int argc, char **argv)
{
  imm::set<int> s;

  while(!std::cin.eof())
    {
      std::string str;
      int i;

      std::cin >> str >> i;

      if(std::cin)
	{
	  if(str == "+")
	    {
	      s.insert(i);
	      s.dump(std::cout);
	      std::cout << std::endl;
	    }
	  else if(str == "-")
	    {
	      s.erase(i);
	      s.dump(std::cout);
	      std::cout << std::endl;
	    }
	  else
	    std::cerr << "Enter '+' or '-' followed by a number." << std::endl;
	}
    }

  return 0;
}

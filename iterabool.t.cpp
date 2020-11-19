// test iterabool
#include <cassert>
#include "iterabool.h"

using namespace iterabool;

int test_constructor()
{
	{
		int i[] = { 1,2,3 };
		counted c(i, 3);
		counted c2{ c };
		c = c2;
		assert(c);
		assert(*c == 1);
		++c;
		assert(c);
		assert(*c == 2);
		++c;
		assert(c);
		assert(*c == 3);
		++c;
		assert(!c);
	}

	return 0;
}

int main()
{
	return 0;
}
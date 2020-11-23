// test iterabool
#include <cassert>
#include "iterabool.h"

using namespace iterabool;

int test_iota()
{
	{
		iota<int> i;
		assert(i);
		iota i2(i);
		assert(i2);
		assert(i2 == i);
		i = i2;
		assert(i == i2);
		assert(*i == 0);
		++i;
		assert(i);
		assert(*i == 1);
	}
	return 0;
}
int test_iota_ = test_iota();

int test_array()
{
	{
		int i[] = { 1,2,3 };
		array c(i);
		array c2{ c };
		assert(c2 == c);
		c = c2;
		assert(c == c2);
		assert(c.size() == 3);
		assert(c);
		assert(*c == 1);
		++c;
		assert(c.size() == 2);
		assert(c);
		assert(*c == 2);
		++c;
		assert(c.size() == 1);
		assert(c);
		assert(*c == 3);
		++c;
		assert(c.size() == 0);
		assert(!c);
		++c;
		assert(c.size() == 0);
		assert(!c);
	}

	return 0;
}
int test_array_ = test_array();

int test_take()
{
	{
		int i[] = { 1,2,3 };
		array a(i);
		size_t n = 2;
		take c(n, a);
		take c2(c);
		assert(c2 == c);
		c = c2;
		assert(c == c2);

		assert(c.size() == 2);
		assert(c);
		assert(*c == 1);
		++c;
		assert(c.size() == 1);
		assert(c);
		assert(*c == 2);
		++c;
		assert(c.size() == 0);
		assert(!c);
		++c;
		assert(c.size() == 0);
		assert(!c);
	}

	return 0;
}
int test_take_ = test_take();

int test_apply()
{
	{
		auto s = take(3, iota<int>{});
		auto t = apply([](int i) { return i * i; }, s);
		assert(t);
		assert(*t == 0);
		++t;
		assert(t);
		assert(*t == 1);
		++t;
		assert(*t == 4);
		++t;
		assert(!t);
		++t;
		assert(!t);
	}

	return 0;
}

int test_fold()
{
	{
		auto f = fold(std::plus<int>{}, take(3, iota<int>(2)), 0);
		auto f2(f);
		f = f2;
		assert(f);
		assert(*f == 2);
		++f;
		assert(f);
		assert(*f == 2 + 3);
		++f;
		assert(f);
		assert(*f == 2 + 3 + 4);
		++f;
		assert(!f);
		++f;
		assert(!f);
	}

	return 0;
}
int test_fold_ = test_fold();

int main()
{
	return 0;
}
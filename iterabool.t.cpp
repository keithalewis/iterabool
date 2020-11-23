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

int test_pow()
{
	{
		auto p = iterabool::pow(2);
		auto p2(p);
		p = p2;
		assert(p);
		assert(*p == 1);
		++p;
		assert(p);
		assert(*p == 2);
		++p;
		assert(p);
		assert(*p == 4);
	}
	return 0;
}
int test_pow_ = test_pow();

int test_factorial()
{
	{
		factorial<int> n;
		auto n2(n);
		n = n2;
		assert(n);
		assert(*n == 1);
		++n;
		assert(n);
		assert(*n == 1);
		++n;
		assert(n);
		assert(*n == 2);
		++n;
		assert(n);
		assert(*n == 6);
		++n;
		assert(n);
	}

	return 0;
}
int test_factorial_ = test_factorial();

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
	{
		assert(0 + 1 + 2 == sum(take(3, iota<int>{})));
		assert(0 == prod(take(3, iota<int>{})));
		assert(1*2*3 == prod(take(3, iota<int>(1))));
	}

	return 0;
}
int test_fold_ = test_fold();

int test_mask()
{
	{
		auto i = take(9, iota<int>{});
		auto p = apply([](int i) { return i % 3 == 0; }, i);
		auto m = mask(p, i);
		auto m2(m);
		m = m2;
		assert(m);
		assert(*m == 0);
		++m;
		assert(m);
		assert(*m == 3);
		++m;
		assert(m);
		assert(*m == 6);
		++m;
		assert(!m);
		++m;
		assert(!m);
	}
	{
		auto i = take(9, iota<int>{});
		auto m = mask(apply([](int i) { return i % 3 == 0; }, i), i);
		auto m2(m);
		m = m2;
		assert(m);
		assert(*m == 0);
		++m;
		assert(m);
		assert(*m == 3);
		++m;
		assert(m);
		assert(*m == 6);
		++m;
		assert(!m);
		++m;
		assert(!m);
	}
	{
		auto m = filter([](int i) { return i % 3 == 0; }, take(9, iota<int>{}));
		auto m2(m);
		m = m2;
		assert(m);
		assert(*m == 0);
		++m;
		assert(m);
		assert(*m == 3);
		++m;
		assert(m);
		assert(*m == 6);
		++m;
		assert(!m);
		++m;
		assert(!m);
	}

	return 0;
}
int test_mask_ = test_mask();

int test_relations()
{
	{
		auto i = iota<int>{};
		auto eq = i < 2;
		assert(eq);
		assert(*eq);
		++eq;
		assert(eq);
		assert(*eq);
		++eq;
		assert(eq);
		assert(!*eq);
		++eq;
		assert(eq);
		assert(!*eq);
	}
	{
		auto i = iota<int>{};
		auto eq = i >= 2;
		assert(eq);
		assert(!*eq);
		++eq;
		assert(eq);
		assert(!*eq);
		++eq;
		assert(eq);
		assert(*eq);
		++eq;
		assert(eq);
		assert(*eq);
	}
	{
		auto i = iota<int>();
		auto eq = i | i >= 2;
		assert(eq);
		assert(*eq == 2);
		++eq;
		assert(eq);
		assert(*eq == 3);
	}

	return 0;
}
int test_relations_ = test_relations();

int test_binop()
{
	{
		auto i0 = iota(0);
		auto i1 = iota(1);
		auto s = binop(std::plus<int>{}, i0, i1);
		auto s2(s);
		s = s2;
		assert(s);
		assert(*s == 0 + 1);
		++s;
		assert(s);
		assert(*s == 1 + 2);
	}
	{
		auto s = iota(0) + iota(1);
		auto s2(s);
		s = s2;
		assert(s);
		assert(*s == 0 + 1);
		++s;
		assert(s);
		assert(*s == 1 + 2);
	}
	{
		auto s = iota(0) * iota(1);
		auto s2(s);
		s = s2;
		assert(s);
		assert(*s == 0 * 1);
		++s;
		assert(s);
		assert(*s == 1 * 2);
	}

	return 0;
}
int test_binop_ = test_binop();

int main()
{
	using iterabool::pow;

	double x = 1;
	auto xn = pow(x);
	auto n_ = factorial<double>{};
	double expx = sum(epsilon(xn/n_));
	double ex = exp(x);
	ex -= expx;

	return 0;
}

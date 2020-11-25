// test iterabool
#include <cassert>
#include "iterabool.h"

using namespace iterabool;

int test_empty()
{
	{
		auto i = take(0, iota(0));
		assert(i == empty(i));
	}
	{
		auto i = take(1, iota(0));
		assert(i != empty(i));
	}

	return 0;
}
int test_empty_ = test_empty();

int test_repeat()
{
	{
		assert(all(eq(repeat(3,2), take(3, c(2)))));
	}
	{
		auto i = take(2, iota(0));
		auto i3 = repeat(3, i);
		auto i32(i3);
		i3 = i32;

		for (auto j : i3) {
			assert(all(eq(j, (i, i, i))));
		}
	}

	return 0;
}
int test_repeat_ = test_repeat();

int test_duplicate()
{
	{
		auto i = take(2, iota(0));
		auto i3 = duplicate(3, i);
		auto i32(i3);
		i3 = i32;

		auto mod = [](int i) { return i % 2; };

		assert(all(eq(i3, apply(mod, take(6, iota(0))))));
	}
	{
		assert(all(eq(duplicate(3, c(1)), repeat(3, 1))));
	}

	return 0;
}
int test_duplicate_ = test_duplicate();

int test_drop()
{
	{
		assert(
			all(
				take(3, 
					eq(drop(2, iota(0)), 
			           iota(2)
				    )
			    )
			)
		);
	}

	return 0;
}
int test_drop_ = test_drop();

int test_c()
{
	{
		c<int> i;
		assert(i);
		c i2(i);
		assert(i2);
		assert(i2 == i);
		i = i2;
		assert(i == i2);
		assert(*i == 0);
		++i;
		assert(i);
		assert(*i == 0);
	}
	{
		for (auto i : take(3, c<int>())) {
			assert(i == 0);
		}
	}
	{
		assert(all(take(3, c(true))));
		assert(!any(take(3, c(false))));
	}

	return 0;
}
int test_c_ = test_c();

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
	{
		int n = 0;
		for (auto i : iota<int>()) {
			assert(n++ == i);
			if (n == 3)
				break;
		}
	}
	{
		iota i = take(3, iota(0));
		auto b = begin(i);
		assert(b == i);
		b = b;
		auto c = *b;
		c = c;
		/*
		int n = 0;
		for (auto j = begin(i); j != end(i); ++j) {
			auto j_ = *j;
			j_ = j_;
		}
		*/
	}

	return 0;
}
int test_iota_ = test_iota();

int test_power()
{
	{
		auto p = power(2);
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
	{
		int n = 1;
		for (auto p : power(2)) {
			assert(p == n);
			n *= 2;
			if (n == 8)
				break;
		}
	}

	return 0;
}
int test_power_ = test_power();

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
	{
		factorial f(0);
		int n = 1, n_ = 1;
		assert(f);
		assert(*f == 1);
		++f;
		for (auto i : f) {
			assert(i == n_);
			++n;
			n_ *= n;
			if (n_ == 120)
				break;
		}
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
	{
		int i[] = { 1,2,3 };
		int n = 1;
		for (auto i : array(i)) {
			assert(i == n++);
		}
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
	{
		int n = 1;
		take t(3, iota(n));
		for (auto i : t) {
			assert(i == n);
			++n;
		}
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
		assert(0 == product(take(3, iota<int>{})));
		assert(1*2*3 == product(take(3, iota<int>(1))));
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
	{
		int m[] = { 0, 1, 0, 0, 1 };
		auto mi = mask(array(m), iota(0));
		auto mi2(mi);
		mi = mi2;
		assert(mi);
		assert(length(mi) == 2);
		assert(mi);
		assert(length(mi) == 2);
		assert(*mi == 1);
		++mi;
		assert(mi);
		assert(*mi == 4);
		++mi;
		assert(!mi);
		++mi;
		assert(!mi);
	}
	{
		int m[] = { 0, 1, 0, 1, 0 };
		auto mi = mask(array(m), iota(0));
		int n = 1;
		for (auto i : mi) {
			assert(i == n);
			n += 2;
		}
		assert(n == 5);
		assert(mi);
		assert(*mi == 1);
		++mi;
		assert(mi);
		assert(*mi == 3);
		++mi;
		assert(!mi);
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
	{
		auto i = take(3, iota<int>{});
		assert(all(eq(i, i)));
		assert(all(ge(i, i)));
		assert(any(eq(i, i)));
		assert(!any(ne(i, i)));
	}
	{
		auto i = take(6, iota(0));
		assert(6 == sum(i | i <= 3));
		assert(9 == sum(i | i > 3));
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

template<class T>
int test_epsilon()
{
	{
		constexpr T eps = std::numeric_limits<T>::epsilon();
		constexpr int digits = std::numeric_limits<T>::digits;
		auto p = power(T(0.5));
		auto e = epsilon(p);
		size_t n;
		n = length(e);
		assert(n == digits);
		n = length(epsilon(p, 2*eps));
		assert(n == digits - 1);
	}

	return 0;
}
int test_epsilon_d = test_epsilon<double>();
int test_epsilon_f = test_epsilon<float>();

int test_concatenate()
{
	{
		auto i = concatenate(take(2, iota<int>()), take(2, iota<int>()));
		auto i2(i);
		i = i2;
		assert(i);
		assert(*i == 0);
		++i;
		assert(i);
		assert(*i == 1);
		++i;
		assert(i);
		assert(*i == 0);
		++i;
		assert(i);
		assert(*i == 1);
		++i;
		assert(!i);
		++i;
		assert(!i);
	}
	{
		auto i_ = take(2, iota<int>());
		auto i = (i_, i_);
		auto i2(i);
		i = i2;
		assert(i);
		assert(*i == 0);
		++i;
		assert(i);
		assert(*i == 1);
		++i;
		assert(i);
		assert(*i == 0);
		++i;
		assert(i);
		assert(*i == 1);
		++i;
		assert(!i);
		++i;
		assert(!i);
	}
	{
		auto i = (take(2, iota<int>()), take(2, iota<int>(2)), take(2, iota<int>(4)));
		auto i2(i);
		i = i2;
		for (int j = 0; j < 6; ++j) {
			assert(i);
			assert(*i == j);
			++i;
		}
		assert(!i);
		++i;
		assert(!i);
	}
	{
		auto i = (take(2, iota<int>()), take(2, iota<int>(2)), take(2, iota<int>(4)));
		auto i6 = take(6, iota<int>());
		//bool b = std::equal(begin(i), end(i), begin(i6), end(i6));
	}

	return 0;
}
int test_concatenate_ = test_concatenate();

int test_tuple()
{
	{
		tuple t(iota(0), iota(1.));
	}

	return 0;
}
int test_tuple_ = test_tuple();

int main()
{
	{
		double x = 1;
		auto xn = power(x);
		auto n_ = factorial<double>{};
		double expx = sum(epsilon(xn / n_));
		double ex = exp(x);
		ex -= expx;
		assert(ex == -2 * std::numeric_limits<double>::epsilon());
	}
	{
		double x = 1;
		assert(exp(x) == sum(epsilon(power(x)/factorial(0.))) - 2 * std::numeric_limits<double>::epsilon());
	}
	/*
	{
		double constexpr eps = std::numeric_limits<double>::epsilon();
		double x = 1;
		auto xn = power(x) / factorial(0.);
		assert(exp(x) == sum(xn | xn > eps) - 2 * eps );
	}
	*/

	return 0;
}

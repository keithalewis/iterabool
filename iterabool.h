// Iterators with operator bool() const
#pragma once
#include <algorithm>
#include <compare>
#include <concepts>
#include <functional>
#include <iterator>
#include <type_traits>

namespace iterabool {

	template<class S>
	concept forward_sequence = requires (S s) {
		//std::is_base_of_v<std::forward_iterator_tag, typename S::interator_category>;
		typename S::iterator_category;
		typename S::value_type;
		{ s.operator bool() } -> std::same_as<bool>;
		{ *s } -> std::convertible_to<typename S::value_type>;
		{ ++s } -> std::same_as<S&>;
	};

	// end sentinal
	struct done { };

	template<forward_sequence S>
	inline S begin(const S& s)
	{
		return s;
	}
	template<forward_sequence S>
	inline done end(const S&)
	{
		return done{};
	}
	template<forward_sequence S>
	inline bool operator==(const S&s, const done&)
	{
		return !s;
	}

	template<forward_sequence S>
	inline constexpr bool all(S s)
	{
		return s.operator bool() ? (*s and all(++s)) : true;
	}

	template<forward_sequence S>
	inline constexpr bool any(S s)
	{
		return s.operator bool() ? (*s or any(++s)) : false;
	}

	// number of items in sequence
	// length(s, length(t)) = length(s) + length(t)
	template<forward_sequence S>
	inline size_t length(S s, size_t n = 0)
	{
		while (s) {
			++n;
			++s;
		}

		return n;
	}

	// drop at most n elements from the beginning
	template<forward_sequence S>
	inline S drop(size_t n, S s)
	{
		while (n and s) {
			--n;
			++s;
		}

		return s;
	}

	// t, t, ...
	template<typename T>
	class c {
		T t;
	public:
		using iterator_concept = std::forward_iterator_tag;
		using iterator_category = std::forward_iterator_tag;
		using value_type = T;

		c(T t = 0)
			: t(t)
		{ }
		c(const c&) = default;
		c& operator=(const c&) = default;
		~c()
		{ }

		//auto operator<=>(const c&) = default;
		bool operator==(const c& i) const
		{
			return t == i.t;
		}

		explicit operator bool() const
		{
			return true;
		}
		T operator*() const
		{
			return t;
		}
		c& operator++()
		{
			return *this;
		}
	};

	// t, t + 1, ...
	template<typename T>
	class iota {
		T t;
	public:
		using iterator_concept = std::forward_iterator_tag;
		using iterator_category = std::forward_iterator_tag;
		using value_type = T;

		iota(T t = 0)
			: t(t)
		{ }
		iota(const iota&) = default;
		iota& operator=(const iota&) = default;
		~iota()
		{ }

		//auto operator<=>(const iota&) = default;
		bool operator==(const iota& i) const
		{
			return t == i.t;
		}

		explicit operator bool() const
		{
			return true;
		}
		T operator*() const
		{
			return t;
		}
		iota& operator++()
		{
			++t;

			return *this;
		}
	};

	// 1, t, t^2, ...
	template<typename T>
	class power {
		T t, tn;
	public:
		using iterator_concept = std::forward_iterator_tag;
		using iterator_category = std::forward_iterator_tag;
		using value_type = T;

		power(T t, T tn = 1)
			: t(t), tn(tn)
		{ }
		power(const power&) = default;
		power& operator=(const power&) = default;
		~power()
		{ }

		//auto operator<=>(const power&) = default;
		bool operator==(const power& i) const
		{
			return t == i.t and tn = i.tn;
		}

		explicit operator bool() const
		{
			return true;
		}
		T operator*() const
		{
			return tn;
		}
		power& operator++()
		{
			tn *= t;

			return *this;
		}
	};

	// 1, 1, 1*2, 1*2*3, ...
	template<typename T>
	class factorial {
		T n, n_;
	public:
		using iterator_concept = std::forward_iterator_tag;
		using iterator_category = std::forward_iterator_tag;
		using value_type = T;

		factorial(T n = 0, T n_ = 1)
			: n(n), n_(n_)
		{ }
		factorial(const factorial&) = default;
		factorial& operator=(const factorial&) = default;
		~factorial()
		{ }

		//auto operator<=>(const factorial&) = default;
		bool operator==(const factorial& i) const
		{
			return n == i.n and n_ = i.n_;
		}

		explicit operator bool() const
		{
			return true;
		}
		T operator*() const
		{
			return n_;
		}
		factorial& operator++()
		{
			n_ *= ++n;

			return *this;
		}
	};

	template<class T>
	class array {
		size_t n;
		T* a;
	public:
		using iterator_concept = std::forward_iterator_tag;
		using iterator_category = std::forward_iterator_tag;
		using value_type = T;

		array(size_t n = 0, T* a = nullptr)
			: n(n), a(a)
		{ }
		template<size_t N>
		array(T(&a)[N])
			: n(N), a(a)
		{ }
		array(const array&) = default;
		array& operator=(const array&) = default;
		~array()
		{ }

		bool operator==(const array& s) const
		{
			return n == s.n and a == s.a;
		}
		//auto operator<=>(const array&) = default;

		// remaining size
		size_t size() const
		{
			return n;
		}

		explicit operator bool() const
		{
			return n != 0;
		}
		value_type operator*() const
		{
			return *a;
		}
		array& operator++()
		{
			if (*this) {
				--n;
				++a;
			}

			return *this;
		}
	};

	// take at most n items from a sequence
	template<forward_sequence S>
	class take {
		size_t n;
		S s;
	public:
		using iterator_concept = typename S::iterator_concept;
		using iterator_category = typename S::iterator_category;
		using value_type = typename S::value_type;

		take()
			: n(0)
		{ }
		take(size_t n, const S& s)
			: n(n), s(s)
		{ }
		take(const take&) = default;
		take& operator=(const take&) = default;
		~take()
		{ }

		bool operator==(const take& t) const
		{
			return operator bool() and t and operator*() == *t;
		}
		//auto operator<=>(const take&) = default;

		// remaining size
		size_t size() const
		{
			return n;
		}

		explicit operator bool() const
		{
			return s.operator bool() and n != 0;
		}
		value_type operator*() const
		{
			return *s;
		}
		take& operator++()
		{
			if (*this) {
				--n;
				++s;
			}

			return *this;
		}
	};

	// truncate when predicate is true
	template<class P, forward_sequence S>
	class until {
		P p;
		S s;
	public:
		using iterator_concept = typename S::iterator_concept;
		using iterator_category = typename S::iterator_category;
		using value_type = typename S::value_type;

		until()
		{ }
		until(const P& p, const S& s)
			: p(p), s(s)
		{ }
		until(const until&) = default;
		until& operator=(const until&) = default;
		~until()
		{ }

		bool operator==(const until& t) const
		{
			return operator bool() and t and operator*() == *t;
		}
		//auto operator<=>(const until&) = default;

		explicit operator bool() const
		{
			return s.operator bool() and static_cast<bool>(!p(*s));
		}
		value_type operator*() const
		{
			return *s;
		}
		until& operator++()
		{
			if (*this) {
				++s;
			}

			return *this;
		}
	};

	// apply function to sequence
	template<class F, forward_sequence S> //,
	class apply {
		using T = typename S::value_type;
		using U = std::invoke_result_t<F, T>;
		F f;
		S s;
	public:
		using iterator_concept = typename S::iterator_concept;
		using iterator_category = typename S::iterator_category;
		using value_type = typename U;

		apply()
		{ }
		apply(const F& f, const S& s)
			: f(f), s(s)
		{ }
		apply(const apply&) = default;
		apply& operator=(const apply&) = default;
		~apply()
		{ }

		// template<class G, forward_sequence R> ... apply<G, R> ???
		bool operator==(const apply& a) const
		{
			return operator bool() and a and operator*() == *a;
		}
		apply begin() const
		{
			return *this;
		}
		const done end() const
		{
			return done{};
		}

		explicit operator bool() const
		{
			return s.operator bool();
		}
		value_type operator*() const
		{
			return f(*s);
		}
		apply& operator++()
		{
			++s;

			return *this;
		}
	};

	// right fold using binop
	template<class Op, forward_sequence S>
	class fold {
		using T = typename S::value_type;
		Op op; // const Op& ???
		S s;
		T t;
	public:
		using iterator_concept = std::forward_iterator_tag;
		using iterator_category = std::forward_iterator_tag;
		using value_type = T;

		fold()
		{ }
		fold(const Op& op, const S& _s, value_type t0)
			: op(op), s(_s), t(t0)
		{
			if (s) {
				t = op(t0, *s);
			}
		}
		fold(const fold&) = default;
		fold& operator=(const fold&) = default;
		~fold()
		{ }

		bool operator==(const fold& f) const
		{
			return operator bool() and f /*and s == f.s*/ and t == s.t;
		}

		explicit operator bool() const
		{
			return s.operator bool();
		}
		value_type operator*() const
		{
			return t;
		}
		fold& operator++()
		{
			if (++s) {
				t = op(t, *s);
			}

			return *this;
		}
	};
	// end result of fold
	template<class Op, forward_sequence S>
	inline typename S::value_type scan(const Op& op, S s, typename S::value_type t)
	{
		while (s) {
			t = op(t, *s);
			++s;
		}

		return t;
	}
	template<forward_sequence S>
	inline typename S::value_type sum(S s, typename S::value_type t = 0)
	{
		using T = typename S::value_type;

		return scan(std::plus<T>{}, s, t);
	}
	template<forward_sequence S>
	inline typename S::value_type product(S s, typename S::value_type t = 1)
	{
		using T = typename S::value_type;

		return scan(std::multiplies<T>{}, s, t);
	}

	// select items using mask
	template<forward_sequence M, forward_sequence S>
	class mask {
		M m;
		S s;
		void next()
		{
			while (m and s) {
				if (!*m) {
					++m;
					++s;
				}
				else {
					return;
				}
			}
		}
	public:
		using iterator_concept = std::forward_iterator_tag;
		using iterator_category = std::forward_iterator_tag;
		using value_type = typename S::value_type;

		mask(const M& m, const S& s)
			: m(m), s(s)
		{
			next();
		}
		mask(const mask&) = default;
		mask& operator=(const mask&) = default;
		~mask()
		{ }

		bool operator==(const mask& ms) const
		{
			return operator bool() and ms /*and *m == *ms.m*/ and *s == *ms.s;
		}

		explicit operator bool() const
		{
			return m.operator bool() and s.operator bool();
		}
		value_type operator*() const
		{
			return *s;
		}
		mask& operator++()
		{
			if (m and s) {
				++m;
				++s;
			}
			next();

			return *this;
		}
	};

	// filter sequence based on predicate
	template<class P, forward_sequence S>
	inline auto filter(const P& p, S s)
	{
		return mask(apply(p, s), s); //??? make class
	}

	// stop when value less than epsilon
	template<class S, class T = S::value_type>
		requires forward_sequence<S> && std::floating_point<T>
	inline auto epsilon(S s, T eps = std::numeric_limits<T>::epsilon())
	{
		auto p = [eps](auto t) { return abs(t) < eps; };

		return until(p, s);
	}

	// apply binop to two sequences
	template<class Op, forward_sequence S, forward_sequence T>
	class binop {
		Op op;
		S s;
		T t;
	public:
		using iterator_concept = std::forward_iterator_tag;
		using iterator_category = std::forward_iterator_tag;
		using value_type = decltype(op(*s, *t));

		binop()
		{ }
		binop(const Op& op, const S& s, const T& t)
			: op(op), s(s), t(t)
		{ }
		binop(const binop&) = default;
		binop& operator=(const binop&) = default;
		~binop()
		{ }

		bool operator==(const binop& b) const
		{
			return operator bool() and b and operator*() == *b;
		}

		explicit operator bool() const
		{
			return s.operator bool() and t.operator bool();
		}
		value_type operator*() const
		{
			return op(*s, *t);
		}
		binop& operator++()
		{
			++s;
			++t;

			return *this;
		}
	};

	template<forward_sequence S0, forward_sequence S1>
	class concatenate {
		S0 s0;
		S1 s1;
	public:
		using iterator_concept = std::forward_iterator_tag;
		using iterator_category = std::forward_iterator_tag;
		using value_type = std::common_type_t<typename S0::value_type, typename S1::value_type>;

		concatenate(const S0& s0, const S1& s1)
			: s0(s0), s1(s1)
		{ }
		concatenate(const concatenate&) = default;
		concatenate& operator=(const concatenate&) = default;
		~concatenate()
		{ }

		bool operator==(const concatenate& s) const
		{
			return s0 == s.s0 and s1 == s.s1;
		}

		explicit operator bool() const
		{
			return s0.operator bool() or s1.operator bool();
		}
		value_type operator*() const
		{
			return s0 ? *s0 : *s1;
		}
		concatenate& operator++()
		{
			if (s0) {
				++s0;
			}
			else if (s1) {
				++s1;
			}

			return *this;
		}
	};

	template<forward_sequence S>
	class lift {
		S s;
		bool once;
	public:
		using iterator_concept = std::forward_iterator_tag;
		using iterator_category = std::forward_iterator_tag;
		using value_type = S;

		lift(const S& s)
			: s(s), once(true)
		{ }
		lift(const lift&) = default;
		lift& operator=(const lift&) = default;
		~lift()
		{ }

		bool operator==(const lift& o) const
		{
			return once == o.once and s = o.s;
		}

		explicit operator bool() const
		{
			return once;
		}
		value_type operator*() const
		{
			return s;
		}
		lift& operator++()
		{
			once = false;

			return *this;
		}
	};

	// sequence of sequences
	template<forward_sequence... Ss>
	class tuple {
		std::tuple<Ss...> ss;
	public:
		tuple(const Ss&... ss)
			: ss(ss...)
		{ }
		tuple(const tuple&) = default;
		tuple& operator=(const tuple&) = default;
		~tuple()
		{ }
	};

	/*
	template<sequence S>
	inline auto coro(S s)
	{
		while (s) {
			yield* s;
			++s;
		}
	}
	*/

}

template<class Op, iterabool::forward_sequence S>
inline auto operator_op(const Op& op, S s, typename S::value_type t)
{
	return iterabool::apply([t, &op](typename S::value_type si) { return op(si, t); }, s);
}

#define OPERATOR_OP(sym, op) \
template<iterabool::forward_sequence S> \
inline auto operator sym (S s, typename S::value_type t) \
{ return operator_op(op<typename S::value_type>{}, s, t); }

OPERATOR_OP(==, std::equal_to);
OPERATOR_OP(!=, std::not_equal_to);
OPERATOR_OP(< , std::less);
OPERATOR_OP(<=, std::less_equal);
OPERATOR_OP(> , std::greater);
OPERATOR_OP(>=, std::greater_equal);

#undef OPERATOR_OP

// sequence conditioned on mask
template<iterabool::forward_sequence S, iterabool::forward_sequence M>
inline auto operator|(const S& s, const M& m)
{
	return iterabool::mask(m, s);
}

#define OPERATOR_BINOP(sym, op) \
template<iterabool::forward_sequence S, iterabool::forward_sequence T> \
inline auto operator sym (const S& s, const T& t) \
{ using U = decltype(op{}(*s,*t)); \
  return iterabool::binop(op<U>{}, s, t); }

OPERATOR_BINOP(+, std::plus);
OPERATOR_BINOP(-, std::minus);
OPERATOR_BINOP(*, std::multiplies);
OPERATOR_BINOP(/, std::divides);
OPERATOR_BINOP(%, std::modulus);

#undef OPERATOR_BINOP

#define OPERATOR_BINOP(sym, op) \
template<iterabool::forward_sequence S, iterabool::forward_sequence T> \
inline auto sym (const S& s, const T& t) \
{ using U = decltype(op{}(*s, *t)); \
return iterabool::binop(op<U>{}, s, t); }

OPERATOR_BINOP(eq, std::equal_to);
OPERATOR_BINOP(ne, std::not_equal_to);
OPERATOR_BINOP(gt, std::greater);
OPERATOR_BINOP(lt, std::less);
OPERATOR_BINOP(ge, std::greater_equal);
OPERATOR_BINOP(le, std::less_equal);

#undef OPERATOR_BINOP

template<iterabool::forward_sequence S0, iterabool::forward_sequence Ss>
inline auto operator,(const S0& s0, const Ss& ss)
{
	return iterabool::concatenate(s0, ss);
}

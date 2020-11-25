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
	struct done { }; //!!! end

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
		bool operator==(const done&) const
		{
			return !operator bool();
		}
		bool operator==(const iota& i) const
		{
			return t == i.t;
		}

		operator bool() const
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
		bool operator==(const done&) const
		{
			return !operator bool();
		}
		bool operator==(const power& i) const
		{
			return t == i.t and tn = i.tn;
		}

		operator bool() const
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
		bool operator==(const done&) const
		{
			return !operator bool();
		}
		bool operator==(const factorial& i) const
		{
			return n == i.n and n_ = i.n_;
		}

		operator bool() const
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

		bool operator==(const done& s) const
		{
			return !operator bool();
		}
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

		operator bool() const
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

		bool operator==(const done&) const
		{
			return !operator bool();
		}
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

		operator bool() const
		{
			return s and n != 0;
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

		bool operator==(const done&) const
		{
			return !operator bool();
		}
		bool operator==(const until& t) const
		{
			return operator bool() and t and operator*() == *t;
		}
		//auto operator<=>(const until&) = default;

		operator bool() const
		{
			return s and !p(*s);
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
	template<class F, class S,
		class T = S::value_type,
		class U = std::invoke_result_t<F, T>>
	class apply {
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
			bool operator==(const done&) const
			{
				return !operator bool();
			}
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

			operator bool() const
			{
				return s;
			}
			U operator*() const
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
	template<class Op, class S, class T = S::value_type>
	class fold {
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

		bool operator==(const done&) const
		{
			return !operator bool();
		}
		bool operator==(const fold& f) const
		{
			return operator bool() and f /*and s == f.s*/ and t == s.t;
		}

		operator bool() const
		{
			return s;
		}
		T operator*() const
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
	inline typename S::value_type sum(S s)
	{
		using T = typename S::value_type;

		return scan(std::plus<T>{}, s, T(0));
	}
	template<forward_sequence S>
	inline typename S::value_type product(S s)
	{
		using T = typename S::value_type;

		return scan(std::multiplies<T>{}, s, T(1));
	}

	template<forward_sequence S>
	inline size_t length(S s, size_t n = 0)
	{
		while (s) {
			++n;
			++s;
		}

		return n;
	}

	// mask sequence
	template<class M, class S, class T = S::value_type>
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
		using value_type = T;

		mask(const M& m, const S& s)
			: m(m), s(s)
		{
			next();
		}
		mask(const mask&) = default;
		mask& operator=(const mask&) = default;
		~mask()
		{ }

		bool operator==(const done&) const
		{
			return !operator bool();
		}
		bool operator==(const mask& ms) const
		{
			return operator bool() and ms /*and *m == *ms.m*/ and *s == *ms.s;
		}

		operator bool() const
		{
			return m and s;
		}
		T operator*() const
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
	template<class P, class S>
	inline auto filter(const P& p, S s)
	{
		return mask(apply(p, s), s);
	}

	// stop when value less than epsilon
	template<class S>
	inline auto epsilon(S s, typename S::value_type eps = 1)
	{
		auto p = [eps](auto t) { return t/eps + 1 == 1; };

		return until(p, s);
	}

	// apply binop to two sequences
	template<class Op, class S, class T>
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

		bool operator==(const done&) const
		{
			return !operator bool();
		}
		bool operator==(const binop& b) const
		{
			return operator bool() and b and operator*() == *b;
		}

		operator bool() const
		{
			return s and t;
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

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
	concept forward_sequence = requires (const S s) {
		typename S::value_type;
		{ s.operator bool() } -> std::same_as<bool>;
		{ s.operator*() } -> std::convertible_to<typename S::value_type>;
		//{ s.operator++() }
	}
	; //&& std::input_iterator<S>;

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

		power(T t)
			: t(t), tn(1)
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

		factorial()
			: n(0), n_(1)
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
		const T* a;
	public:
		using iterator_concept = std::forward_iterator_tag;
		using iterator_category = std::forward_iterator_tag;
		using value_type = T;

		array()
			: n(0), a(nullptr)
		{ }
		template<size_t N>
		array(const T(&a)[N])
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

		operator bool() const
		{
			return n != 0;
		}
		/*typename value_type*/ int operator*() const
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
			return n == t.n and s == t.s;
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
		Op op;
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
	template<class S, class T = S::value_type>
	inline auto epsilon(S s, T eps = std::numeric_limits<T>::epsilon())
	{
		auto p = [eps](T t) { return (t * (1 + eps)) != t; };

		return filter(p, s);
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

#if 0

	template<sequence S>
	class counted {
		S s;
		size_t n;
	public:
		using value_type = typename std::iterator_traits<S>::value_type;
		counted()
		{ }
		counted(S s, size_t n)
			: s(std::move(s)), n(n)
		{ }
		counted(const counted&) = default;
		counted& operator=(const counted&) = default;
		counted(counted&&)  = default;
		counted& operator=(counted&&) = default;
		~counted()
		{ }

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
			return *s;
		}
		counted operator++()
		{
			if (n != 0) {
				--n;
				++s;
			}

			return *this;
		}
	};


#endif
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

OPERATOR_OP(== , std::equal_to);
OPERATOR_OP(!= , std::not_equal_to);
OPERATOR_OP(< , std::less);
OPERATOR_OP(<= , std::less_equal);
OPERATOR_OP(> , std::greater);
OPERATOR_OP(>= , std::greater_equal);

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

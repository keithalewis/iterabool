// Iterators with operator bool() const
#pragma once
#include <algorithm>
#include <compare>
#include <concepts>
#include <functional>
#include <iterator>
#include <type_traits>

namespace iterabool {

	// forward iterator with operator bool to detect the end
	template<class S>
	concept forward_sequence = requires (S s) {
		//std::is_base_of_v<std::forward_iterator_tag, typename S::interator_category>;
		typename S::iterator_concept;
		typename S::iterator_category;
		typename S::value_type;
		{ s.operator bool() } -> std::same_as<bool>;
		{ *s } -> std::convertible_to<typename S::value_type>;
		{ ++s } -> std::same_as<S&>;
	};

	// sequence with no elements used as end sentinal
	template<forward_sequence S>
	struct empty : public S {
		using iterator_concept = std::forward_iterator_tag;
		using iterator_category = std::forward_iterator_tag;
		using value_type = typename S::value_type;

		empty()
		{ }
		empty(const S&)
		{ }

		bool operator==(const S& s) const
		{
			return !s;
		}

		explicit operator bool() const
		{
			return false;
		}
		value_type operator*() const
		{
			return value_type{};
		}
		empty& operator++()
		{
			return *this;
		}
	};


	// STL friendly
	template<forward_sequence S>
	inline S begin(const S& s)
	{
		return s;
	}
	// end sentinal
	template<forward_sequence S>
	inline empty<S> end(const S& s)
	{
		return empty(s);
	}
	// for (auto i = begin(); i != end(); ++i) works
	template<forward_sequence S>
	inline bool operator==(const S& s, const empty<S>&)
	{
		return !s;
	}

	// every element is convertible to true
	template<forward_sequence S>
	inline constexpr bool all(S s)
	{
		return s ? (*s and all(++s)) : true;
	}

	// some element is convertible to true
	template<forward_sequence S>
	inline constexpr bool any(S s)
	{
		return s ? (*s or any(++s)) : false;
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
	template<forward_sequence S>
	inline size_t size(S s, size_t n = 0)
	{
		return length(s, n);
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

	// constant: t, t, ...
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

	// increment: t, t + 1, ...
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

	// powers of t: tn, tn*t, tn*t^2, ...
	template<typename T>
	class power {
		T t, tn;
	public:
		using iterator_concept = std::forward_iterator_tag;
		using iterator_category = std::forward_iterator_tag;
		using value_type = T;

		power(T t = 1, T tn = 1)
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
	template<class T>
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

		auto operator<=>(const factorial&) const = default;

		explicit operator bool() const
		{
			return true;
		}
		value_type operator*() const
		{
			return n_;
		}
		factorial& operator++()
		{
			n_ *= ++n;

			return *this;
		}
	};

	// Pochhammer symbol: x(x + k)(x + 2k)...
	template<class T>
	class pochhammer {
		T x, k, x_, k_;
	public:
		using iterator_concept = std::forward_iterator_tag;
		using iterator_category = std::forward_iterator_tag;
		using value_type = T;

		pochhammer(T x, T k)
			: x(x), k(k), x_(x), k_(0)
		{ }
		pochhammer(const pochhammer&) = default;
		pochhammer& operator=(const pochhammer&) = default;
		~pochhammer()
		{ }

		auto operator<=>(const pochhammer&) const = default;

		explicit operator bool() const
		{
			return true;
		}
		value_type operator*() const
		{
			return x_;
		}
		pochhammer& operator++()
		{
			k_ += k;
			x_ *= (x - k_);

			return *this;
		}
	};

	// repeat t n times more efficiently than take(n,c(t))
	template<class T>
	class repeat {
		size_t n;
		T t;
	public:
		using iterator_concept = std::forward_iterator_tag;
		using iterator_category = std::forward_iterator_tag;
		using value_type = T;

		repeat()
		{ }
		repeat(size_t n, const T& t)
			: n(n), t(t)
		{ }
		repeat(const repeat&) = default;
		repeat& operator=(const repeat&) = default;
		~repeat()
		{ }

		auto operator<=>(const repeat&) const = default;

		explicit operator bool() const
		{
			return n != 0;
		}
		value_type operator*() const
		{
			return t;
		}
		repeat& operator++()
		{
			if (*this) {
				--n;
			}
			
			return *this;
		}
	};

	// duplicate sequence n times, duplicate(n, unit(t)) == repeat(n, t)
	template<forward_sequence S>
	class duplicate {
		size_t n;
		S s0, s;
	public:
		using iterator_concept = std::forward_iterator_tag;
		using iterator_category = std::forward_iterator_tag;
		using value_type = typename S::value_type;

		duplicate(size_t n, const S& s)
			: n(n), s0(s), s(s)
		{ }
		duplicate(const duplicate&) = default;
		duplicate& operator=(const duplicate&) = default;
		~duplicate()
		{ }

		bool operator<=>(const duplicate&) const = default;

		explicit operator bool() const
		{
			return n != 0 and s;
		}
		value_type operator*() const
		{
			return *s;
		}
		duplicate& operator++()
		{
			if (s) {
				++s;
			}
			else {
				if (n != 0) {
					--n;
					s = s0;
				}
			}

			return *this;
		}
	};

	// convert array to sequence
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

		auto operator<=>(const array&) const = default;

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
		value_type& operator*()
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

		auto operator<=>(const take&) const = default;

		// remaining size
		size_t size() const
		{
			return n;
		}

		explicit operator bool() const
		{
			return n != 0 and s;
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
			return *this and t and *s == *t.s; // p == s.p???
		}

		explicit operator bool() const
		{
			return s and !p(*s);
		}
		value_type operator*() const
		{
			return *s;
		}
		until& operator++()
		{
			if (s) {
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

		bool operator==(const apply& a) const
		{
			return *this and a and operator*() == *a; // f == a.f???
		}
		apply begin() const
		{
			return *this;
		}

		explicit operator bool() const
		{
			return !!s;
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
			return *this and f /*and s == f.s*/ and t == s.t;
		}

		explicit operator bool() const
		{
			return !!s;
		}
		value_type operator*() const
		{
			return t;
		}
		fold& operator++()
		{
			if (s and ++s) {
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
	template<forward_sequence S, class T = typename S::value_type>
	inline T sum(S s, T t = 0)
	{
		return scan(std::plus<T>{}, s, t);
	}
	template<forward_sequence S, class T = typename S::value_type>
	inline T product(S s, T t = 1)
	{
		return scan(std::multiplies<T>{}, s, t);
	}

	// select items using mask
	template<forward_sequence M, forward_sequence S>
	class mask {
		M m;
		S s;
		void next()
		{
			// skip false mask values
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

		mask()
		{ }
		mask(const M& m, const S& s)
			: m(m), s(s)
		{
			next();
		}
		mask(const mask&) = default;
		mask& operator=(const mask&) = default;
		~mask()
		{ }

		auto operator<=>(const mask&) const = default;

		explicit operator bool() const
		{
			return m and s;
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
		auto p = [eps](T t) { return abs(t) < eps; };

		return until(p, s);
	}

	// apply binary operator to two sequences
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
			return *this and b and operator*() == *b; // op???
		}

		explicit operator bool() const
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

	// concatenate two sequences
	template<forward_sequence S0, forward_sequence S1,
		class T = std::common_type_t<typename S0::value_type, typename S1::value_type>>
	class concatenate {
		S0 s0;
		S1 s1;
	public:
		using iterator_concept = std::forward_iterator_tag;
		using iterator_category = std::forward_iterator_tag;
		using value_type = T;

		concatenate(const S0& s0, const S1& s1)
			: s0(s0), s1(s1)
		{ }
		concatenate(const concatenate&) = default;
		concatenate& operator=(const concatenate&) = default;
		~concatenate()
		{ }

		auto operator<=>(const concatenate& s) const = default;

		explicit operator bool() const
		{
			return s0 or s1;
		}
		value_type operator*() const
		{
			if (s0) {
				return *s0;
			}

			return *s1;
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

	// sequence of one sequence
	template<forward_sequence S>
	class unit {
		S s;
	public:
		using iterator_concept = std::forward_iterator_tag;
		using iterator_category = std::forward_iterator_tag;
		using value_type = S;

		unit(const S& s)
			: s(s)
		{ }
		unit(const unit&) = default;
		unit& operator=(const unit&) = default;
		~unit()
		{ }

		bool operator<=>(const unit&) const = default;

		explicit operator bool() const
		{
			return !!s;
		}
		value_type operator*() const
		{
			return s;
		}
		value_type& operator*()
		{
			return s;
		}
		unit& operator++()
		{
			s = empty(s);

			return *this;
		}
		unit& operator++(int)
		{
			++s;

			return *this;
		}
	};

	// sequence of sequences
	template<forward_sequence... Ss>
	inline auto sequence(const Ss&... ss)
	{
		return (...,unit(ss));
	}

	// sequences tuples
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
		// operator bool() const ???
		// operator*() const ???
		// operator++() ???
	};

	// sequence of sequences to a sequence
	template<forward_sequence S>
	class flatten {
		S s;
	public:
		using iterator_concept = std::forward_iterator_tag;
		using iterator_category = std::forward_iterator_tag;
		using value_type = decltype(*(*s));

		flatten(const S& s)
			: s(s)
		{ }

		explicit operator bool() const
		{
			return !!s;
		}
		value_type operator*() const
		{
			return *(*s);
		}
		flatten& operator++()
		{
			++s; // increment sequence in unit

			return *this;
		}
		
	};

	/* make a sequence concurrent
	template<sequence S>
	inline auto coro(S s)
	{
		while (s) {
			co_yield* s;
			++s;
		}
	}
	*/

}

template<class Op, iterabool::forward_sequence S>
inline auto operator_binop(const Op& op, S s, typename S::value_type t)
{
	return iterabool::apply([t, &op](typename S::value_type si) { return op(si, t); }, s);
}

#define OPERATOR_OP(sym, op) \
template<iterabool::forward_sequence S> \
inline auto operator sym (S s, typename S::value_type t) \
{ return operator_binop(op<typename S::value_type>{}, s, t); }

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

template<iterabool::forward_sequence S>
inline auto exp(const S& s)
{
	return iterabool::apply(exp, s);
}
// etc...

template<iterabool::forward_sequence S0, iterabool::forward_sequence Ss>
inline auto operator,(const S0& s0, const Ss& ss)
{
	return iterabool::concatenate(s0, ss);
}

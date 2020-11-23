// Iterators with operator bool() const
#pragma once
#include <algorithm>
#include <compare>
#include <concepts>
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

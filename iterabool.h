// Iterators with operator bool() const
#pragma once
#include <algorithm>
#include <concepts>
#include <iterator>
#include <type_traits>

namespace iterabool {

	template<typename S>
	concept sequence = std::input_iterator<S> 
		&& requires (S s) { { !s }; };
		/*
		requires (S s) {
		typename std::iterator_traits<S>::value_type;
		{ s.operator bool() } -> std::same_as<bool>;
		{ s.operator*() } -> std::convertible_to<typename std::iterator_traits<S>::value_type>;
		{ s.operator++() } -> std::convertible_to<S&>;
	};
		*/

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
		counted(counted&&) noexcept = default;
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



}

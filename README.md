# iterabool

This has got to be the dumbest name for a library ever. Better suggestions welcome.

This header only library uses C++ concepts to make iterators easier to use by
adding the member function `operator bool() const`. This is not a new idea.
Many other languages use something similar, in particular C# where it is used
to make [`LINQ`](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/)
the coolest thing since button shoes.

I do a lot of numerical programming and I work very hard to be lazy. Using streams rather
than arrays makes C++ much more expressive. For example, `sum(epsilon(power(x)/factorial()))`
computes `exp(x)` to machine precision using exp(x) = &Sigma;<sub>n >= 0</sub> x<sup>n</sup>/n!.

This poorly named library supplies the usual stream functions like `apply`, (right) `fold`, `mask`, and `filter`
and overloads some operators for syntactic sugar. For example, 
`mask(apply([](auto i) { return i <= 2; } iota()), iota()))`, or equivalently 
`filter([](auto i) { return i <= 2; }, iota())`,
returns the elements `{0, 1, ...}` that are less than or equal to 2. 
It can also be written `iota() | iota() <= 2` where `|` is read _given_.

All iterabools have `begin() const` and `end() const` member functions so they can be
used with STL routines or in range based for loops. The `end()` function returns
a class called `done`. Every iterabool has an `operator==(const done&) const` that
returns `!operator bool()`.

The real beauty part of this is asynchronous programming becomes trivial using coroutines.
If `s` is iterabool then
```
auto coro(S s) {
	while (s) {
		co_yield *s;
		++s;
	}
}
```
leverages the genious compiler writers handiwork to create multitasking code.
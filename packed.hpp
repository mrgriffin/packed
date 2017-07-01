// Instead of tag types we could use BOOST_STRONG_TYPEDEF. This would
// give us an interface that looks a lot more like std::variant and
// std::tuple.
// #include <boost/serialization/strong_typedef.hpp>

#include <climits>
#include <cstdint>
#include <type_traits>

namespace packed
{

namespace detail
{

template<uint64_t N>
struct of_size {
    using type = std::conditional_t<N - 1 <= UINT8_MAX, std::uint8_t,
                 std::conditional_t<N - 1 <= UINT16_MAX, std::uint16_t,
                 std::conditional_t<N - 1 <= UINT32_MAX, std::uint32_t,
                 std::conditional_t<N - 1 <= UINT64_MAX, std::uint64_t,
                 void>>>>;
};

template<uint64_t N>
using of_size_t = typename of_size<N>::type;

template<typename K, typename... _Ts>
struct lookup;

template<typename K, typename... _Ts>
using lookup_t = typename lookup<K, _Ts...>::type;

template<typename K, typename... _Ts>
constexpr auto lookup_v = lookup<K, _Ts...>::value;

// FIXME: Use is_same_v on a C++17 stdlib.
// TODO: Use enable_if to factor out is_same (and provide a nice error
//       message via static_assert).

template<typename K, typename N, typename T>
struct lookup<K, N, T> {
    using type = std::conditional_t<std::is_same<K, N>::value, T, void>;
    static constexpr auto value = std::conditional_t<std::is_same<K, N>::value,
                                                     std::integral_constant<int, 0>,
                                                     // TODO: Compile error if not found.
                                                     std::integral_constant<int, -1>>::value;

    // HINT: C++17 introduces typename F, previously it had to be class F.
    template<template<typename...> typename F>
    using with_tail = F<>;
};

template<typename K, typename N, typename T, typename... _Ts>
struct lookup<K, N, T, _Ts...> {
    using type = std::conditional_t<std::is_same<K, N>::value,
                                    T,
                                    lookup_t<K, _Ts...>>;
    static constexpr auto value = std::is_same<K, N>::value
                                ? 0
                                : lookup_v<K, _Ts...> != -1
                                  ? 1 + lookup_v<K, _Ts...>
                                  : -1;

    template<template<typename...> typename F>
    using with_tail = std::conditional_t<std::is_same<K, N>::value,
                                         F<_Ts...>,
                                         typename lookup<K, _Ts...>::template with_tail<F>>;
};

// FIXME: This shouldn't be necessary?
template<template<typename...> typename F, typename K, typename... _Ts>
using _with_tail_v = typename lookup<K, _Ts...>::template with_tail<F>;

template<template<typename...> typename F, typename K, typename... _Ts>
constexpr auto lookup_with_tail_v = _with_tail_v<F, K, _Ts...>::value;

template<typename... _Ts>
struct sum_size;

template<typename... _Ts>
constexpr auto sum_size_v = sum_size<_Ts...>::value;

template<>
struct sum_size<> {
    static constexpr auto value = 0;
};

template<typename _, typename T, typename... _Ts>
struct sum_size<_, T, _Ts...> {
    static constexpr auto value = T::values + sum_size_v<_Ts...>;
};

template<typename... _Ts>
struct product_size;

template<typename... _Ts>
constexpr auto product_size_v = product_size<_Ts...>::value;

template<>
struct product_size<> {
    static constexpr auto value = 0;
};

template<typename _, typename T>
struct product_size<_, T> {
    static constexpr auto value = T::values;
};

template<typename _, typename T, typename... _Ts>
struct product_size<_, T, _Ts...> {
    static constexpr auto value = T::values * product_size_v<_Ts...>;
};

}

// TODO: Surely int64_t is more appropriate?
template<uint64_t I, uint64_t J=0>
struct range {
    // HINT: Swap I and J to have range<N> == range<0, N>.
    static constexpr auto values = (I < J ? J - I : I - J);

    constexpr range(uint64_t v) : value(v) {}
    constexpr operator detail::of_size_t<values>() const noexcept { return value; }
    detail::of_size_t<values> value;
};

template<typename... NTs>
class variant {
public:
    static constexpr auto values = detail::sum_size_v<NTs...>;
    using underlying_type = detail::of_size_t<values>;

    template<typename K>
    static constexpr variant make(detail::lookup_t<K, NTs...> t) {
        // TODO: Use brace initialization (must silence warning about narrowing).
        return detail::lookup_with_tail_v<detail::sum_size, K, NTs...> + underlying_type(t);
    }

// HINT: Used for tests.
//private:
    underlying_type value;

private:
    constexpr variant(underlying_type v) : value(v) {}
};

template<typename N, typename T, typename... NTs>
class struct_ {
    template<typename, typename, typename...>
    friend class struct_;

public:
    static constexpr auto values = detail::product_size_v<N, T, NTs...>;
    using underlying_type = detail::of_size_t<values>;

    template<typename... Ts>
    static constexpr struct_ make(T t, Ts... ts) {
        return _make(0, t, ts...);
    }

// HINT: Used for tests.
//private:
    underlying_type value;

private:
    constexpr struct_(underlying_type v) : value(v) {}

    template<typename... Ts>
    static constexpr underlying_type _make(underlying_type a, T t, Ts... ts) {
        return struct_<NTs...>::_make(a * T::values + underlying_type(t), ts...);
    }

    static constexpr underlying_type _make(underlying_type a, T t) {
        static_assert(sizeof...(NTs) == 0);
        return a * T::values + underlying_type(t);
    }
};

// TODO: std::array-like.
// TODO: Support (ab)using spare bits in pointers with struct_<T*, U>?
//       alignof tells us how many bits are required. Note this requires
//       us to transform compacted values (i.e. shift), but that would
//       be needed for correct enum support anyway.

}

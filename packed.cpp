#include <climits>
#include <stdexcept>
#include <type_traits>

namespace packed
{

// TODO: Use std::bad_variant_access.
class bad_variant_access : public std::exception {};

namespace detail
{

template<std::size_t N>
struct at_least {
    using type = std::conditional_t<N - 1 <= UINT8_MAX, std::uint8_t,
                 std::conditional_t<N - 1 <= UINT16_MAX, std::uint16_t,
                 std::conditional_t<N - 1 <= UINT32_MAX, std::uint32_t,
                 std::conditional_t<N - 1 <= UINT64_MAX, std::uint64_t,
                 void>>>>;
};

template<std::size_t N>
using at_least_t = typename at_least<N>::type;

template<typename D>
struct cardinality {
    static constexpr std::size_t value = D::cardinality;
};

template<typename T>
struct cardinality<T*> {
    // TODO: static_assert that value is big enough to hold rhs (char* overflows!)
    static constexpr std::size_t value = 1ul << (sizeof(T*) * CHAR_BIT - alignof(T) + 1);
};

template<>
struct cardinality<bool> {
    static constexpr std::size_t value = 2;
};

template<typename D>
static constexpr auto cardinality_v = cardinality<D>::value;

template<typename... Ts>
struct sum_cardinality;

template<typename... Ts>
constexpr auto sum_cardinality_v = sum_cardinality<Ts...>::value;

template<>
struct sum_cardinality<> {
    static constexpr auto value = 0;
};

template<typename T, typename... Ts>
struct sum_cardinality<T, Ts...> {
    static constexpr auto value = cardinality_v<T> + sum_cardinality_v<Ts...>;
};

template<typename... Ts>
struct product_cardinality;

template<typename... Ts>
constexpr auto product_cardinality_v = product_cardinality<Ts...>::value;

template<>
struct product_cardinality<> {
    static constexpr auto value = 0;
};

template<typename T>
struct product_cardinality<T> {
    static constexpr auto value = cardinality_v<T>;
};

template<typename T, typename... Ts>
struct product_cardinality<T, Ts...> {
    static constexpr auto value = cardinality_v<T> * product_cardinality_v<Ts...>;
};

template<typename I, typename T>
class has_pack {
private:
    using True = char[1];
    using False = char[2];

    template<typename U>
    static True& test(decltype(&U::template pack<I>));

    template<typename U>
    static False& test(...);

public:
    static constexpr auto value = sizeof test<T>(nullptr) == sizeof(True);
};

template<typename I, typename T>
static constexpr auto has_pack_v = has_pack<I, T>::value;

template<typename I, typename D, typename=std::enable_if_t<has_pack_v<I, D>>>
constexpr I pack(auto v)
{
    return D::template pack<I>(v);
}

// HINT: reinterpret_cast isn't valid in constexpr.
// TODO: Use is_pointer_v.
template<typename I, typename P, typename=std::enable_if_t<std::is_pointer<P>::value>>
I pack(P v)
{
    return reinterpret_cast<I>(v) >> (alignof(std::remove_pointer_t<P>) - 1);
}

template<typename I>
I pack(bool v)
{
    return v;
}

template<typename I, typename T>
class has_unpack {
private:
    using True = char[1];
    using False = char[2];

    template<typename U>
    static True& test(decltype(&U::template unpack<I>));

    template<typename U>
    static False& test(...);

public:
    static constexpr auto value = sizeof test<T>(nullptr) == sizeof(True);
};

template<typename I, typename T>
static constexpr auto has_unpack_v = has_unpack<I, T>::value;

// TODO: Make the enable_if a default argument for consistency.
template<typename I, typename D, typename=std::enable_if_t<has_unpack_v<I, D>>>
constexpr auto unpack(I i)
{
    return D::template unpack(i);
}

template<typename I, typename P>
P unpack(I i, std::enable_if_t<std::is_pointer<P>::value, int> _=0)
{
    return reinterpret_cast<P>(i << (alignof(std::remove_pointer_t<P>) - 1));
}

template<typename I, typename B>
B unpack(I i, std::enable_if_t<std::is_same<B, bool>::value, int> _=0)
{
    return i;
}

template<typename I, typename T, typename U, typename... Us>
constexpr T variant_get(I i, std::enable_if_t<std::is_same<T, U>::value, int> _=0)
{
    if (i < cardinality_v<T>) return unpack<I, T>(i);
    else throw bad_variant_access();
}

template<typename I, typename T, typename U, typename... Us>
constexpr T variant_get(I i, std::enable_if_t<!std::is_same<T, U>::value, int> _=0)
{
    return variant_get<I, T, Us...>(i - cardinality_v<U>);
}

template<typename I, typename T, typename U, typename... Us>
constexpr T tuple_get(I i, std::enable_if_t<std::is_same<T, U>::value, int> _=0)
{
    return unpack<I, T>(i % cardinality_v<T>);
}

template<typename I, typename T, typename U, typename... Us>
constexpr T tuple_get(I i, std::enable_if_t<!std::is_same<T, U>::value, int> _=0)
{
    return tuple_get<I, T, Us...>(i / cardinality_v<U>);
}

}

template<typename T, T B, T E=0>
class range {
private:
    static constexpr auto lo = B < E ? B : E;
    static constexpr auto hi = B < E ? E : B;

public:
    static constexpr auto cardinality = hi - lo;

public:
    // TODO: Make these non-member functions?
    template<typename I>
    static constexpr I pack(T v) {
        if (v < lo || v > hi) throw std::out_of_range("");
        return v - lo;
    }

    template<typename I>
    static constexpr T unpack(I i) {
        return i + lo;
    }
};

template<typename E, E... Vs>
class enum_ {
private:
    static constexpr E vs[] = { Vs... };

public:
    static constexpr auto cardinality = sizeof...(Vs);

    template<typename I>
    static constexpr I pack(E v) {
        std::size_t i = 0;
        for (; i < cardinality; ++i) if (vs[i] == v) return i;
        if (i == cardinality) throw std::out_of_range("");
    }

    template<typename I>
    static constexpr E unpack(I i) {
        return vs[i];
    }
};

template<typename... Ts>
class variant {
public:
    static constexpr auto cardinality = detail::sum_cardinality_v<Ts...>;

    // HINT: Used by std::get.
    using underlying_type = detail::at_least_t<cardinality>;
private:

public:
    template<typename T>
    explicit constexpr variant(T t) : value(pack<T, Ts...>(t)) {}

    // HINT: Used for tests.
    underlying_type value;
private:

    // TODO: Use if constexpr.
    template<typename T, typename U, typename... Us>
    static constexpr underlying_type pack(T t, std::enable_if_t<std::is_same<T, U>::value, int> _=0) {
        return detail::pack<underlying_type>(t);
    }

    template<typename T, typename U, typename... Us>
    static constexpr underlying_type pack(T t, std::enable_if_t<!std::is_same<T, U>::value, int> _=0) {
        return detail::cardinality_v<U> + pack<T, Us...>(t);
    }
};

template<typename... Ts>
class tuple {
public:
    static constexpr auto cardinality = detail::product_cardinality_v<Ts...>;

    explicit constexpr tuple(Ts... ts) : value(pack<Ts...>(ts...)) {}

    // HINT: Used by std::get.
    // FIXME: This overflows if all 2**64 values are needed!
    using underlying_type = detail::at_least_t<cardinality>;

    underlying_type value;
private:

    template<typename T>
    static constexpr underlying_type pack(T t) {
        return detail::pack<underlying_type>(t);
    }

    template<typename T, typename... _Ts>
    static constexpr underlying_type pack(T t, _Ts... ts) {
        return pack<T>(t) | detail::cardinality_v<T> * pack<_Ts...>(ts...);
    }
};

}

namespace std
{

template<typename T, typename... Us>
constexpr T get(packed::variant<Us...> const& v)
{
    // TODO: Throw if wrong variant is set without recursing.
    return ::packed::detail::variant_get<typename packed::variant<Us...>::underlying_type, T, Us...>(v.value);
}

template<typename T, typename... Us>
constexpr T get(packed::tuple<Us...> const& t)
{
    return ::packed::detail::tuple_get<typename packed::tuple<Us...>::underlying_type, T, Us...>(t.value);
}

}

static_assert(packed::detail::cardinality_v<packed::range<int, 4>> == 4);
static_assert(alignof(short) == 2);
static_assert(sizeof(short*) == 8);
static_assert(packed::detail::cardinality_v<short*> == 1ul << 63);
static_assert(packed::detail::pack<int, packed::range<int, 2, 4>>(3) == 1);
static_assert(packed::detail::unpack<int, packed::range<int, 2, 4>>(1) == 3);

using MaybeShort = packed::variant<bool, short*>;
static_assert(sizeof(MaybeShort) == sizeof(short*));

//using V = packed::variant<packed::range<int, 4>, bool>;
//static_assert(std::get<packed::range<int, 4>>(V::make<packed::range<int, 4>>(1)) == 1);

using TaggedShort = packed::tuple<bool, short*>;
static_assert(sizeof(TaggedShort) == sizeof(short*));

using T = packed::tuple<packed::range<int, 4>, packed::range<int, 3>, bool>;
static_assert(std::get<bool>(T(0, 0, true)) == true);

#include <cassert>
#include <cstdint>
#include <cstdio>
int main()
{
    assert((packed::detail::pack<std::intptr_t, short*>((short*)0x4) == 2));
    assert((packed::detail::unpack<std::intptr_t, short*>(2) == (short*)0x4));
    assert(MaybeShort(true).value == 1);
    assert(MaybeShort((short*)0x04).value == 4);
    assert(std::get<bool>(MaybeShort(true)) == true);
    assert(std::get<short*>(MaybeShort((short*)0x04)) == (short*)0x04);
    try {
        std::get<short*>(MaybeShort(true));
        assert(false);
    } catch (packed::bad_variant_access& e) {
    }

    auto ts = TaggedShort(true, (short*)0x04);
    assert(std::get<bool>(ts) == true);
    assert(std::get<short*>(ts) == (short*)0x04);
    return 0;
}

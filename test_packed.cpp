#include <iterator>
#include <type_traits>
#include "packed.hpp"

struct tag;
static_assert(packed::detail::sum_size<>::value == 0);
static_assert(packed::detail::sum_size<tag, packed::range<4>>::value == 4);
static_assert(packed::detail::sum_size<tag, packed::range<4>,
                                       tag, packed::range<2>>::value == 6);

static_assert(packed::variant<tag, packed::range<4>,
                              tag, packed::range<2>>::values == 6);

static_assert(packed::struct_<tag, packed::range<4>>::values == 4);
static_assert(packed::struct_<tag, packed::range<4>,
                              tag, packed::range<2>>::values == 8);
static_assert(sizeof(packed::struct_<tag, packed::range<16>>) == 1);
static_assert(sizeof(packed::struct_<tag, packed::range<16>,
                                     tag, packed::range<16>>) == 1);
static_assert(sizeof(packed::variant<tag, packed::range<1>,
                                     tag, packed::struct_<tag, packed::range<16>,
                                                          tag, packed::range<16>>>) == 2);
static_assert(sizeof(packed::variant<tag, packed::range<1>,
                                     tag, packed::range<257>>) == 2);

struct tag2;
static_assert(std::is_same<packed::detail::lookup_t<tag, tag, int>, int>::value);
static_assert(std::is_same<packed::detail::lookup_t<tag, tag, int,
                                                         tag2, char>, int>::value);
static_assert(std::is_same<packed::detail::lookup_t<tag2, tag, int,
                                                          tag2, char>, char>::value);
static_assert(packed::detail::lookup_v<tag, tag, int> == 0);

static_assert(packed::detail::lookup<tag,
                                     tag, packed::range<4>,
                                     tag2, packed::range<2>>
              ::with_tail<packed::detail::sum_size>::value == 2);
static_assert(packed::detail::lookup<tag2,
                                     tag, packed::range<4>,
                                     tag2, packed::range<2>>
              ::with_tail<packed::detail::sum_size>::value == 0);

using R = packed::range<4>;
constexpr auto r1 = R(0);
constexpr int r2 = R(1);
static_assert(r1 == 0);
static_assert(r2 == 1);

class tag3;
using T = packed::variant<tag, packed::range<2>,
                          tag2, packed::range<4>,
                          tag3, packed::range<3>>;
static_assert(T::make<tag>(1).value == 3 + 4 + 1); // HINT: 4 is the number of values to the right.
static_assert(T::make<tag2>(2).value == 3 + 2);
static_assert(T::make<tag3>(0).value == 0);

using T2 = packed::struct_<tag, packed::range<2>,
                           tag2, packed::range<4>>;
constexpr auto t2 = T2::make(1, 2);
static_assert(t2.value == ((1 * 4) | 2));

constexpr bool test_struct_representations_distinct() {
    T2::underlying_type ts[2 * 4] = {};
    for (int i = 0; i < 2; ++i) {
        for (int j = 0; j < 4; ++j) {
            ts[i * 4 + j] = T2::make(i, j).value;
        }
    }

    // TODO: Surely we can do better than O(n²)?
    for (auto it = std::begin(ts); it != std::end(ts); ++it) {
        for (auto jt = it + 1; jt != std::end(ts); ++jt) {
            if (*it == *jt) return false;
        }
    }

    return true;
}
static_assert(test_struct_representations_distinct());

#include <cstdio>
#include <utility>
std::pair<T2, T2> mkpair(int i, int j) {
    T2 t1 = T2::make(i, j);
    T2 t2 = T2::make(j, i);
    return {t1, t2};
}

int __attribute__((optimize("O0"))) main()
{
    int i, j;
    std::scanf("%d %d", &i, &j);
    auto ts = mkpair(i, j);
    std::printf("%d %d\n", ts.first.value, ts.second.value);
}

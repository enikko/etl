/**
 * @file strong_type.hpp
 * 
 *
 * @author Erik Nikko <65210465+enikko@users.noreply.github.com>
 *
 **/

/**
 * MIT License
 *
 * Copyright (c) 2021 Erik Nikko
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 **/

#include <type_traits>
#include <utility>

namespace etl {
namespace st_op {
template<typename to>
struct cast {};

struct add {};
struct subtract {};
struct unary_subtract {};
struct multiply {};
struct divide {};
struct arithmetic {};

struct less {};
struct less_than_equal {};
struct equal {};
struct not_equal {};
struct greater_than_equal {};
struct greater {};
struct default_comparison {};

struct assign {};

template<typename op, typename... ops>
struct has_op;

template<typename op, typename... ops>
struct has_op<op, op, ops...> : std::true_type {};

template<typename op>
struct has_op<op> : std::false_type {};

template<typename op, typename check_op, typename... ops>
struct has_op<op, check_op, ops...> : has_op<op, ops...> {};

template<typename op, typename... ops>
constexpr bool has_op_v = has_op<op, ops...>::value;
} // namespace st_op

template<typename marker, typename rep, typename... ops>
class strong_type {
public:
  using representation_t = rep;
  using marker_t = marker;

  explicit strong_type(const rep& value) : value_{value} {}
  explicit strong_type(rep&& value) : value_{std::move(value)} {}
  strong_type(const strong_type& other) = default;
  strong_type(strong_type&& other) noexcept = default;

  template<typename other_marker,
           std::enable_if_t<st_op::has_op_v<st_op::assign, ops...> &&
                                std::is_same_v<marker, other_marker>,
                            bool> = true>
  marker& operator=(const strong_type<other_marker, rep, ops...>& other) {
    value_ = other.value_;
    return *static_cast<marker*>(this);
  }

  template<typename other_marker,
           std::enable_if_t<st_op::has_op_v<st_op::assign, ops...> &&
                                std::is_same_v<marker, other_marker>,
                            bool> = true>
  marker& operator=(strong_type<other_marker, rep, ops...>&& other) {
    value_ = std::move(other.value_);
    return *static_cast<marker*>(this);
  }

  explicit operator rep() const { return value_; }

  template<
      typename to,
      std::enable_if_t<st_op::has_op_v<st_op::cast<to>, ops...>, bool> = true>
  explicit operator to() const {
    return to{value_};
  }

  template<typename other_marker,
           std::enable_if_t<(st_op::has_op_v<st_op::add, ops...> ||
                             st_op::has_op_v<st_op::arithmetic, ops...>)&&std::
                                is_same_v<marker, other_marker>,
                            bool> = true>
  marker operator+(const strong_type<other_marker, rep, ops...>& other) const {
    return marker{value_ + other.value_};
  }

  template<
      typename other_marker,
      std::enable_if_t<(st_op::has_op_v<st_op::add, ops...> ||
                        st_op::has_op_v<st_op::arithmetic, ops...>)&&st_op::
                               has_op_v<st_op::assign, ops...> &&
                           std::is_same_v<marker, other_marker>,
                       bool> = true>
  marker& operator+=(const strong_type<other_marker, rep, ops...>& other) {
    value_ += other.value_;
    return *static_cast<marker*>(this);
  }

  template<typename other_marker,
           std::enable_if_t<(st_op::has_op_v<st_op::subtract, ops...> ||
                             st_op::has_op_v<st_op::arithmetic, ops...>)&&std::
                                is_same_v<marker, other_marker>,
                            bool> = true>
  marker operator-(const strong_type<other_marker, rep, ops...>& other) const {
    return marker{value_ - other.value_};
  }

  template<
      typename other_marker,
      std::enable_if_t<(st_op::has_op_v<st_op::subtract, ops...> ||
                        st_op::has_op_v<st_op::arithmetic, ops...>)&&st_op::
                               has_op_v<st_op::assign, ops...> &&
                           std::is_same_v<marker, other_marker>,
                       bool> = true>
  marker& operator-=(const strong_type<other_marker, rep, ops...>& other) {
    value_ -= other.value_;
    return *static_cast<marker*>(this);
  }

  template<typename other_marker,
           std::enable_if_t<(st_op::has_op_v<st_op::multiply, ops...> ||
                             st_op::has_op_v<st_op::arithmetic, ops...>)&&std::
                                is_same_v<marker, other_marker>,
                            bool> = true>
  marker operator*(const strong_type<other_marker, rep, ops...>& other) const {
    return marker{value_ * other.value_};
  }

  template<
      typename other_marker,
      std::enable_if_t<(st_op::has_op_v<st_op::multiply, ops...> ||
                        st_op::has_op_v<st_op::arithmetic, ops...>)&&st_op::
                               has_op_v<st_op::assign, ops...> &&
                           std::is_same_v<marker, other_marker>,
                       bool> = true>
  marker& operator*=(const strong_type<other_marker, rep, ops...>& other) {
    value_ *= other.value_;
    return *static_cast<marker*>(this);
  }

  template<typename other_marker,
           std::enable_if_t<(st_op::has_op_v<st_op::divide, ops...> ||
                             st_op::has_op_v<st_op::arithmetic, ops...>)&&std::
                                is_same_v<marker, other_marker>,
                            bool> = true>
  marker operator/(const strong_type<other_marker, rep, ops...>& other) const {
    return marker{value_ / other.value_};
  }

  template<
      typename other_marker,
      std::enable_if_t<(st_op::has_op_v<st_op::divide, ops...> ||
                        st_op::has_op_v<st_op::arithmetic, ops...>)&&st_op::
                               has_op_v<st_op::assign, ops...> &&
                           std::is_same_v<marker, other_marker>,
                       bool> = true>
  marker& operator/=(const strong_type<other_marker, rep, ops...>& other) {
    value_ /= other.value_;
    return *static_cast<marker*>(this);
  }

  template<typename other_marker = marker,
           std::enable_if_t<(st_op::has_op_v<st_op::unary_subtract, ops...> ||
                             st_op::has_op_v<st_op::arithmetic, ops...>)&&std::
                                is_same_v<marker, other_marker>,
                            bool> = true>
  typename strong_type<other_marker, rep, ops...>::marker_t operator-() const {
    return marker{-value_};
  }

  template<typename other_marker,
           std::enable_if_t<
               (st_op::has_op_v<st_op::less, ops...> ||
                st_op::has_op_v<st_op::default_comparison,
                                ops...>)&&std::is_same_v<marker, other_marker>,
               bool> = true>
  bool operator<(const strong_type<other_marker, rep, ops...>& other) const {
    return value_ < other.value_;
  }

  template<typename other_marker,
           std::enable_if_t<
               (st_op::has_op_v<st_op::less_than_equal, ops...> ||
                st_op::has_op_v<st_op::default_comparison,
                                ops...>)&&std::is_same_v<marker, other_marker>,
               bool> = true>
  bool operator<=(const strong_type<other_marker, rep, ops...>& other) const {
    return value_ <= other.value_;
  }

  template<typename other_marker,
           std::enable_if_t<
               (st_op::has_op_v<st_op::equal, ops...> ||
                st_op::has_op_v<st_op::default_comparison,
                                ops...>)&&std::is_same_v<marker, other_marker>,
               bool> = true>
  bool operator==(const strong_type<other_marker, rep, ops...>& other) const {
    return value_ == other.value_;
  }

  template<typename other_marker,
           std::enable_if_t<
               (st_op::has_op_v<st_op::not_equal, ops...> ||
                st_op::has_op_v<st_op::default_comparison,
                                ops...>)&&std::is_same_v<marker, other_marker>,
               bool> = true>
  bool operator!=(const strong_type<other_marker, rep, ops...>& other) const {
    return value_ != other.value_;
  }

  template<typename other_marker,
           std::enable_if_t<
               (st_op::has_op_v<st_op::greater, ops...> ||
                st_op::has_op_v<st_op::default_comparison,
                                ops...>)&&std::is_same_v<marker, other_marker>,
               bool> = true>
  bool operator>(const strong_type<other_marker, rep, ops...>& other) const {
    return value_ > other.value_;
  }

  template<typename other_marker,
           std::enable_if_t<
               (st_op::has_op_v<st_op::greater_than_equal, ops...> ||
                st_op::has_op_v<st_op::default_comparison,
                                ops...>)&&std::is_same_v<marker, other_marker>,
               bool> = true>
  bool operator>=(const strong_type<other_marker, rep, ops...>& other) const {
    return value_ >= other.value_;
  }

private:
  representation_t value_;
};

template<typename marker, typename rep, typename... ops>
using all_op_strong_type =
    strong_type<marker, rep, st_op::arithmetic, st_op::assign,
                st_op::default_comparison, ops...>;
} // namespace etl

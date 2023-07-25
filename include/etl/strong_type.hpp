/**
 * @file strong_type.hpp
 * This file contains the interface and implementation of the strong types.
 *
 * @author Erik Nikko <65210465+enikko@users.noreply.github.com>
 *
 **/

/**
 * MIT License
 *
 * Copyright (c) 2022 Erik Nikko
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

/// @brief The st_op namespace contains template markers to select what operators
///        that should be generated for a strong type.
namespace st_op {
/// @brief Marker to generate a explicit cast operator to @p to.
/// @tparam to The type for which the explicit cast operator should
///            be generated.
/// @pre @p to implements an, explicit, single argument constructor
///      that takes the representation of the strong type.
template<typename to>
struct cast {};

/// @brief Marker to generate plus operator for the strong type.
/// @pre The representation of the strong type supports the plus
///      operator.
struct plus {};
/// @brief Marker to generate unary plus operator for the strong type.
/// @pre The representation of the strong type supports the unary plus
///      operator.
struct unary_plus {};
/// @brief Marker to generate subtract operator for the strong type.
/// @pre The representation of the strong type supports the subtract
///      operator.
struct subtract {};
/// @brief Marker to generate unary subtract operator for the strong type.
/// @pre The representation of the strong type supports the unary subtract
///      operator.
struct unary_subtract {};
/// @brief Marker to generate multiplication operator for the strong type.
/// @pre The representation of the strong type supports the multiplication
///      operator.
struct multiplication {};
/// @brief Marker to generate division operator for the strong type.
/// @pre The representation of the strong type supports the division
///      operator.
struct division {};
/// @brief Marker to generate modulo operator for the strong type.
/// @pre The representation of the strong type supports the modulo
///      operator.
struct modulo {};
/// @brief Marker to generate bitwise not operator for the strong type.
/// @pre The representation of the strong type supports the bitwise not
///      operator.
struct bit_not {};
/// @brief Marker to generate bitwise and operator for the strong type.
/// @pre The representation of the strong type supports the bitwise and
///      operator.
struct bit_and {};
/// @brief Marker to generate bitwise or operator for the strong type.
/// @pre The representation of the strong type supports the bitwise or
///      operator.
struct bit_or {};
/// @brief Marker to generate bitwise xor operator for the strong type.
/// @pre The representation of the strong type supports the bitwise xor
///      operator.
struct bit_xor {};
/// @brief Marker to generate bitwise right shift operator with an unsigned
///        integer as the right hand side operand for the strong type.
/// @pre The representation of the strong type supports the bitwise right shift
///      operator with unsigned integer operand.
struct rshift {};
/// @brief Marker to generate bitwise left shift operator with an unsigned
///        integer as the right hand side operand for the strong type.
/// @pre The representation of the strong type supports the bitwise left shift
///      operator with unsigned integer operand.
struct lshift {};

struct bitwise_shift {};

/// @brief Marker to generate all unary arithmetic operators
///        (see https://en.cppreference.com/w/cpp/language/operator_arithmetic).
/// @pre The representation of the strong type supports the
///      operators.
struct unary_arithmetic {};

/// @brief Marker to generate all additive arithmetic operators
///        (see https://en.cppreference.com/w/cpp/language/operator_arithmetic).
/// @pre The representation of the strong type supports the
///      operators.
struct additive_arithmetic {};

/// @brief Marker to generate all multiplicative arithmetic operators
///        (see https://en.cppreference.com/w/cpp/language/operator_arithmetic).
/// @pre The representation of the strong type supports the
///      operators.
struct multiplicative_arithmetic {};

/// @brief Marker to generate all bitwise logic operators
///        (see https://en.cppreference.com/w/cpp/language/operator_arithmetic).
/// @pre The representation of the strong type supports the
///      operators.
struct bitwise_logic {};

/// @brief Marker to generate all arithmetic operators
///        (see https://en.cppreference.com/w/cpp/language/operator_arithmetic).
/// @pre The representation of the strong type supports the
///      operators.
struct arithmetic {};

// TODO implement
struct lnot {};
struct land {};
struct lor {};

struct logical {};

struct pre_inc {};
struct post_inc {};
struct pre_dec {};
struct post_dec {};

struct inc_dec {};

struct subscription {};
struct indirection {};
struct address_of {};
struct member_of_pointer {};
struct member_access {};

/// @brief Marker to generate a less than operator for the strong type.
/// @pre The representation of the strong type supports the less than
///      operators.
struct less {};
/// @brief Marker to generate a less than or equal operator for the strong type.
/// @pre The representation of the strong type supports the less than or equal
///      operators.
struct less_than_equal {};
/// @brief Marker to generate an equality operator for the strong type.
/// @pre The representation of the strong type supports the equality
///      operators.
struct equal {};
/// @brief Marker to generate an inequality operator for the strong type.
/// @pre The representation of the strong type supports the inequality
///      operators.
struct not_equal {};
/// @brief Marker to generate a greater than or equal operator for the strong type.
/// @pre The representation of the strong type supports the greater than or equal
///      operators.
struct greater_than_equal {};
/// @brief Marker to generate a greater than operator for the strong type.
/// @pre The representation of the strong type supports the greater than
///      operators.
struct greater {};
/// @brief Marker to generate all comparison operators for the strong type.
/// @pre The representation of the strong type supports all comparison
///      operators.
struct default_comparison {};

/// TODO: Add pre and post increment

/// @brief Maker to generate assignment operators for the strong type.
///        This also includes arithmetic and binary compound-assignments if the
///        used operator is defined. For example, if a strong type has the
///        plus and assign markers, then the +, = and += operators will be defined.
/// @pre The representation of the strong type supports the operators.
struct assign {};
} // namespace st_op

/// @brief A strong type template to create domain types for applications.
/// @tparam marker The domain type.
/// @tparam rep The type used to represent the domain type.
/// @tparam ops The operators that should be supported by the domain type.
template<typename domain_type, typename rep, typename... ops>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
class strong_type {
private:
  /// @brief Checks whether operator @p op is among @p to_check.
  /// @tparam op The operator to look for.
  /// @tparam to_check The operators to look among.
  template<typename op, typename... to_check>
  struct has_op : std::false_type {};
  template<typename op, typename... to_check>
  struct has_op<op, op, to_check...> : std::true_type {};
  template<typename op, typename check, typename... to_check>
  struct has_op<op, check, to_check...> : has_op<op, to_check...> {};

  /// @brief Checks whether the domain type should have @p op defined.
  template<typename op>
  constexpr static bool has_op_v = has_op<op, ops...>::value;

public:
  /// @brief The representation type that is used for the strong type.
  using representation_t = rep;
  /// @brief The domain type itself.
  using domain_t = domain_type;

  /// @brief Constructs a new strong type from the representation.
  /// @param value The initial value.
  explicit strong_type(const rep& value) : value_{value} {}
  explicit strong_type(rep&& value) : value_{std::move(value)} {}

  strong_type(const strong_type& other) = default;
  strong_type(strong_type&& other) noexcept = default;
  ~strong_type() = default;

  template<typename other_marker,
           std::enable_if_t<has_op_v<st_op::assign> &&
                                std::is_same_v<domain_t, other_marker>,
                            bool> = true>
  domain_t& operator=(const strong_type<other_marker, rep, ops...>& other) {
    value_ = other.value_;
    return *static_cast<domain_t*>(this);
  }

  template<typename other_marker,
           std::enable_if_t<has_op_v<st_op::assign> &&
                                std::is_same_v<domain_t, other_marker>,
                            bool> = true>
  domain_t& operator=(strong_type<other_marker, rep, ops...>&& other) {
    value_ = std::move(other.value_);
    return *static_cast<domain_t*>(this);
  }

  explicit operator rep() const { return value_; }

  template<typename to,
           std::enable_if_t<has_op_v<st_op::cast<to>>, bool> = true>
  explicit operator to() const {
    return to{value_};
  }

  template<
      typename other_marker,
      std::enable_if_t<
          (has_op_v<st_op::plus> || has_op_v<st_op::additive_arithmetic> ||
           has_op_v<st_op::arithmetic>)&&std::is_same_v<domain_t, other_marker>,
          bool> = true>
  domain_t
  operator+(const strong_type<other_marker, rep, ops...>& other) const {
    return domain_t{value_ + other.value_};
  }

  template<typename other_marker,
           std::enable_if_t<
               (has_op_v<st_op::plus> || has_op_v<st_op::additive_arithmetic> ||
                has_op_v<st_op::arithmetic>)&&has_op_v<st_op::assign> &&
                   std::is_same_v<domain_t, other_marker>,
               bool> = true>
  domain_t& operator+=(const strong_type<other_marker, rep, ops...>& other) {
    value_ += other.value_;
    return *static_cast<domain_t*>(this);
  }

  template<
      typename other_marker = domain_t,
      std::enable_if_t<
          (has_op_v<st_op::unary_plus> || has_op_v<st_op::unary_arithmetic> ||
           has_op_v<st_op::arithmetic>)&&std::is_same_v<domain_t, other_marker>,
          bool> = true>
  typename strong_type<other_marker, rep, ops...>::domain_t operator+() const {
    return domain_t{+value_};
  }

  template<
      typename other_marker,
      std::enable_if_t<
          (has_op_v<st_op::subtract> || has_op_v<st_op::additive_arithmetic> ||
           has_op_v<st_op::arithmetic>)&&std::is_same_v<domain_t, other_marker>,
          bool> = true>
  domain_t
  operator-(const strong_type<other_marker, rep, ops...>& other) const {
    return domain_t{value_ - other.value_};
  }

  template<
      typename other_marker,
      std::enable_if_t<(has_op_v<st_op::subtract> ||
                        has_op_v<st_op::additive_arithmetic> ||
                        has_op_v<st_op::arithmetic>)&&has_op_v<st_op::assign> &&
                           std::is_same_v<domain_t, other_marker>,
                       bool> = true>
  domain_t& operator-=(const strong_type<other_marker, rep, ops...>& other) {
    value_ -= other.value_;
    return *static_cast<domain_t*>(this);
  }

  template<
      typename other_marker = domain_t,
      std::enable_if_t<
          (has_op_v<st_op::unary_subtract> ||
           has_op_v<st_op::unary_arithmetic> ||
           has_op_v<st_op::arithmetic>)&&std::is_same_v<domain_t, other_marker>,
          bool> = true>
  typename strong_type<other_marker, rep, ops...>::domain_t operator-() const {
    return domain_t{-value_};
  }

  template<
      typename other_marker,
      std::enable_if_t<
          (has_op_v<st_op::multiplication> ||
           has_op_v<st_op::multiplicative_arithmetic> ||
           has_op_v<st_op::arithmetic>)&&std::is_same_v<domain_t, other_marker>,
          bool> = true>
  domain_t
  operator*(const strong_type<other_marker, rep, ops...>& other) const {
    return domain_t{value_ * other.value_};
  }

  template<
      typename other_marker,
      std::enable_if_t<(has_op_v<st_op::multiplication> ||
                        has_op_v<st_op::multiplicative_arithmetic> ||
                        has_op_v<st_op::arithmetic>)&&has_op_v<st_op::assign> &&
                           std::is_same_v<domain_t, other_marker>,
                       bool> = true>
  domain_t& operator*=(const strong_type<other_marker, rep, ops...>& other) {
    value_ *= other.value_;
    return *static_cast<domain_t*>(this);
  }

  template<
      typename other_marker,
      std::enable_if_t<
          (has_op_v<st_op::division> ||
           has_op_v<st_op::multiplicative_arithmetic> ||
           has_op_v<st_op::arithmetic>)&&std::is_same_v<domain_t, other_marker>,
          bool> = true>
  domain_t
  operator/(const strong_type<other_marker, rep, ops...>& other) const {
    return domain_t{value_ / other.value_};
  }

  template<
      typename other_marker,
      std::enable_if_t<(has_op_v<st_op::division> ||
                        has_op_v<st_op::multiplicative_arithmetic> ||
                        has_op_v<st_op::arithmetic>)&&has_op_v<st_op::assign> &&
                           std::is_same_v<domain_t, other_marker>,
                       bool> = true>
  domain_t& operator/=(const strong_type<other_marker, rep, ops...>& other) {
    value_ /= other.value_;
    return *static_cast<domain_t*>(this);
  }

  template<
      typename other_marker,
      std::enable_if_t<
          (has_op_v<st_op::modulo> ||
           has_op_v<st_op::multiplicative_arithmetic> ||
           has_op_v<st_op::arithmetic>)&&std::is_same_v<domain_t, other_marker>,
          bool> = true>
  domain_t
  operator%(const strong_type<other_marker, rep, ops...>& other) const {
    return domain_t{value_ % other.value_};
  }

  template<
      typename other_marker,
      std::enable_if_t<(has_op_v<st_op::modulo> ||
                        has_op_v<st_op::multiplicative_arithmetic> ||
                        has_op_v<st_op::arithmetic>)&&has_op_v<st_op::assign> &&
                           std::is_same_v<domain_t, other_marker>,
                       bool> = true>
  domain_t& operator%=(const strong_type<other_marker, rep, ops...>& other) {
    value_ %= other.value_;
    return *static_cast<domain_t*>(this);
  }

  ////////// Bitwise logic arithmetic operators
  template<
      typename other_marker = domain_t,
      std::enable_if_t<
          (has_op_v<st_op::bit_not> || has_op_v<st_op::bitwise_logic> ||
           has_op_v<st_op::arithmetic>)&&std::is_same_v<domain_t, other_marker>,
          bool> = true>
  typename strong_type<other_marker, rep, ops...>::domain_t operator~() const {
    return domain_t{~value_};
  }

  template<
      typename other_marker,
      std::enable_if_t<
          (has_op_v<st_op::bit_and> || has_op_v<st_op::bitwise_logic> ||
           has_op_v<st_op::arithmetic>)&&std::is_same_v<domain_t, other_marker>,
          bool> = true>
  domain_t
  operator&(const strong_type<other_marker, rep, ops...>& other) const {
    return domain_t{value_ & other.value_};
  }

  template<
      typename other_marker,
      std::enable_if_t<
          (has_op_v<st_op::bit_or> || has_op_v<st_op::bitwise_logic> ||
           has_op_v<st_op::arithmetic>)&&std::is_same_v<domain_t, other_marker>,
          bool> = true>
  domain_t
  operator|(const strong_type<other_marker, rep, ops...>& other) const {
    return domain_t{value_ | other.value_};
  }

  template<
      typename other_marker,
      std::enable_if_t<
          (has_op_v<st_op::bit_xor> || has_op_v<st_op::bitwise_logic> ||
           has_op_v<st_op::arithmetic>)&&std::is_same_v<domain_t, other_marker>,
          bool> = true>
  domain_t
  operator^(const strong_type<other_marker, rep, ops...>& other) const {
    return domain_t{value_ | other.value_};
  }

  ////////// Bitwise shift operators
  template<typename uint,
           std::enable_if_t<
               (has_op_v<st_op::lshift> || has_op_v<st_op::bitwise_shift> ||
                has_op_v<st_op::arithmetic>)&&std::is_unsigned_v<uint>,
               bool> = true>
  domain_t operator<<(uint shift) const {
    return domain_t{value_ << shift};
  }

  template<typename uint,
           std::enable_if_t<
               (has_op_v<st_op::rshift> || has_op_v<st_op::bitwise_shift> ||
                has_op_v<st_op::arithmetic>)&&std::is_unsigned_v<uint>,
               bool> = true>
  domain_t operator>>(uint shift) const {
    return domain_t{value_ >> shift};
  }

  ////////// Comparison operators
  template<typename other_marker,
           std::enable_if_t<(has_op_v<st_op::less> ||
                             has_op_v<st_op::default_comparison>)&&std::
                                is_same_v<domain_t, other_marker>,
                            bool> = true>
  bool operator<(const strong_type<other_marker, rep, ops...>& other) const {
    return value_ < other.value_;
  }

  template<typename other_marker,
           std::enable_if_t<(has_op_v<st_op::less_than_equal> ||
                             has_op_v<st_op::default_comparison>)&&std::
                                is_same_v<domain_t, other_marker>,
                            bool> = true>
  bool operator<=(const strong_type<other_marker, rep, ops...>& other) const {
    return value_ <= other.value_;
  }

  template<typename other_marker,
           std::enable_if_t<(has_op_v<st_op::equal> ||
                             has_op_v<st_op::default_comparison>)&&std::
                                is_same_v<domain_t, other_marker>,
                            bool> = true>
  bool operator==(const strong_type<other_marker, rep, ops...>& other) const {
    return value_ == other.value_;
  }

  template<typename other_marker,
           std::enable_if_t<(has_op_v<st_op::not_equal> ||
                             has_op_v<st_op::default_comparison>)&&std::
                                is_same_v<domain_t, other_marker>,
                            bool> = true>
  bool operator!=(const strong_type<other_marker, rep, ops...>& other) const {
    return value_ != other.value_;
  }

  template<typename other_marker,
           std::enable_if_t<(has_op_v<st_op::greater> ||
                             has_op_v<st_op::default_comparison>)&&std::
                                is_same_v<domain_t, other_marker>,
                            bool> = true>
  bool operator>(const strong_type<other_marker, rep, ops...>& other) const {
    return value_ > other.value_;
  }

  template<typename other_marker,
           std::enable_if_t<(has_op_v<st_op::greater_than_equal> ||
                             has_op_v<st_op::default_comparison>)&&std::
                                is_same_v<domain_t, other_marker>,
                            bool> = true>
  bool operator>=(const strong_type<other_marker, rep, ops...>& other) const {
    return value_ >= other.value_;
  }

private:
  representation_t value_;
};

/// @brief Defines a strong type with all operators enabled.
/// @tparam domain_type The domain type.
/// @tparam rep The representation used for the domain type.
/// @tparam casts Markers for the casting operators that the domain type should support.
template<typename domain_type, typename rep, typename... casts>
using all_op_strong_type =
    strong_type<domain_type, rep, st_op::arithmetic, st_op::assign,
                st_op::default_comparison, casts...>;
} // namespace etl

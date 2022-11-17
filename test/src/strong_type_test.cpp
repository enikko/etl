/**
 * @file strong_type.hpp
 * 
 *
 * @author Erik Nikko <65210465+enikko@users.noreply.github.com>
 *
 **/

//  MIT License
//
//  Copyright (c) 2022 Erik Nikko
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#include "etl/strong_type.hpp"

#include <catch2/catch_test_macros.hpp>
#include <cstdint>

struct meter;
struct second;
struct meter_per_second;

struct centimeter : etl::all_op_strong_type<centimeter, int> {
  using strong_type::strong_type;
  explicit operator meter() const;
};

struct meter : etl::all_op_strong_type<meter, int> {
  using strong_type::strong_type;
  explicit operator centimeter() const {
    return centimeter{static_cast<int>(*this) * 100};
  }
  meter_per_second operator/(const second& time) const;
  using strong_type::operator/;
};

centimeter::operator meter() const {
  return meter{static_cast<int>(*this) / 100};
}

struct second : etl::all_op_strong_type<second, int> {
  using strong_type::strong_type;
  meter operator*(const meter_per_second& speed) const;
  friend meter operator*(const meter_per_second& speed, const second& time) {
    return time * speed;
  }
  using strong_type::operator+;
};

struct meter_per_second : etl::all_op_strong_type<meter_per_second, int> {
  using strong_type::strong_type;
};

meter second::operator*(const meter_per_second& speed) const {
  return meter{static_cast<int>(*this) * static_cast<int>(speed)};
}

meter_per_second meter::operator/(const second& time) const {
  return meter_per_second{static_cast<int>(*this) / static_cast<int>(time)};
}

struct distance;

struct length
    : etl::all_op_strong_type<length, meter, etl::st_op::cast<distance>> {
  using strong_type::strong_type;
};

struct distance : etl::strong_type<distance, meter, etl::st_op::cast<length>,
                                   etl::st_op::default_comparison> {
  using strong_type::strong_type;
};

// Signature of user defined literals must be
// for at least some compilers.

// NOLINTNEXTLINE(google-runtime-int)
centimeter operator""_cm(unsigned long long value) {
  return centimeter{static_cast<int>(value)};
}

// NOLINTNEXTLINE(google-runtime-int)
meter operator""_m(unsigned long long value) {
  return meter{static_cast<int>(value)};
}

// NOLINTNEXTLINE(google-runtime-int)
second operator""_s(unsigned long long value) {
  return second{static_cast<int>(value)};
}

// NOLINTNEXTLINE(google-runtime-int)
meter_per_second operator""_mps(unsigned long long value) {
  return meter_per_second{static_cast<int>(value)};
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
TEST_CASE("strong_type: nested types", "[strong_type]") {
  SECTION("Construction") { length l{1_m}; }
  SECTION("Access") {
    length l1{42_m};
    CHECK(static_cast<meter>(l1) == 42_m);
    meter m1{static_cast<meter>(l1)};
    CHECK(static_cast<int>(m1) == 42);
    length l2{1_m};
    CHECK(static_cast<meter>(l2) == 1_m);
    meter m2{static_cast<meter>(l2)};
    CHECK(static_cast<int>(m2) == 1);
  }
  SECTION("Operators") {
    // Just make sure that some operators work here
    // Rest is checked throughly in basic operators
    length l1{42_m};
    length l2{2_m};
    CHECK(l1 / l2 == length{21_m});
    CHECK(static_cast<distance>(l1) == distance{42_m});
    CHECK(100_cm == static_cast<centimeter>(1_m));
    CHECK(static_cast<meter>(200_cm) == 2_m);
  }
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
TEST_CASE("strong_type: basic operators", "[strong_type]") {
  SECTION("Arithmetic") {
    meter test{0_m};
    CHECK(test == 0_m);
    test = 2_m + 2_m;
    CHECK(test == 4_m);
    test = 2_m - 4_m;
    CHECK(test == -2_m);
    test = -4_m;
    CHECK(test == -4_m);
    test = 2_m * 3_m;
    CHECK(test == 6_m);
    test = 4_m / 2_m;
    CHECK(test == 2_m);
  }
  SECTION("Assign") {
    meter test{0_m};
    CHECK(test == 0_m);
    test = 2_m;
    CHECK(test == 2_m);
    test += 2_m;
    CHECK(test == 4_m);
    test *= 2_m;
    CHECK(test == 8_m);
    test /= 4_m;
    CHECK(test == 2_m);
    test -= 2_m;
    CHECK(test == 0_m);
  }
  SECTION("Comparison") {
    CHECK(1_m == 1_m);
    CHECK(!(1_m == 2_m));
    CHECK(!(2_m != 2_m));
    CHECK(2_m != 3_m);
    CHECK(1_m < 3_m);
    CHECK(!(1_m < 1_m));
    CHECK(!(1_m < 0_m));
    CHECK(!(-1_m > 2_m));
    CHECK(!(-1_m > -1_m));
    CHECK(-1_m > -3_m);
    CHECK(1_m <= 3_m);
    CHECK(1_m <= 1_m);
    CHECK(!(1_m <= 0_m));
    CHECK(!(-1_m >= 2_m));
    CHECK(-1_m >= -1_m);
    CHECK(-1_m >= -3_m);
  }
  SECTION("User defined") {
    CHECK(4_m / 2_s == 2_mps);
    CHECK(4_mps * 2_s == 8_m);
  }
}

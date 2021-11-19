/**
 * @file tokenizer.hpp
 *
 * @brief 
 *
 * @author Erik Nikko
 * Contact: 65210465+enikko@users.noreply.github.com
 *
 **/

//  MIT License
//
//  Copyright (c) 2021 Erik Nikko
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

#include <cstddef>
#include <istream>
#include <tuple>
#include <type_traits>
#include <vector>

namespace etl {
// Intended usage
// struct data { int x; char dom; };
// etl::Tokenizer<etl::UnboundStream<data, int, etl::trim, char>>{etl::cin};

namespace parser {

template<typename Parser, typename... Ts>
using target_t = decltype(Parser::target(std::declval<std::istream&>(),
                                         std::declval<std::tuple<Ts...>&>()));

template<typename Target, typename... Parsers>
struct compound {
private:
  template<typename... Ps>
  struct parse_impl;
  template<typename P, typename... Ps>
  struct parse_impl<P, Ps...> {
    template<typename... Ts>
    static auto parse(std::istream& in, std::tuple<Ts...>& stm) {
      auto res = P::parse(in);
      if constexpr (std::is_same_v<decltype(res), void>) {
        return parse_impl<Ps...>::parse(in, stm);
      } else {
        return std::tuple_cat(std::make_tuple(res),
                              parse_impl<Ps...>::parse(in, stm));
      }
    }
  };
  template<typename P>
  struct parse_impl<P> {
    template<typename... Ts>
    static auto parse(std::istream& in, std::tuple<Ts...>& stm) {
      auto res = P::parse(in, stm);
      if constexpr (std::is_same_v<decltype(res), void>) {
        return std::tuple<>{};
      } else
        return std::make_tuple(res);
    }
  };

public:
  template<typename... Ts>
  static Target parse(std::istream& in, std::tuple<Ts...>& stm) {
    return std::make_from_tuple<Target>(parse_impl<Parsers...>::parse(in, stm));
  }
};

template<typename T>
struct atomic {
  template<typename... Ts>
  static T parse(std::istream& in, const std::tuple<Ts...>& /*stm*/) {
    T obj{};
    in >> obj;
    return obj;
  }
};

template<typename Marker, bool Discard = true,
         typename T = decltype(std::declval<Marker>().value)>
struct stm_writer : public atomic<T> {
  using ret = std::conditional_t<Discard, void, T>;
  template<typename... Ts>
  static ret parse(std::istream& in, std::tuple<Ts...>& stm) {
    T data{};
    in >> data;
    std::get<Marker>(stm).value = data;
    if constexpr (!Discard)
      return data;
  }
};

namespace size {
struct inf {};

template<typename T>
struct is_inf : std::false_type {};
template<>
struct is_inf<inf> : std::true_type {};
template<typename T>
constexpr bool is_inf_v = is_inf<T>::value;

template<size_t Size>
struct fixed {
  static constexpr size_t value = Size;
};

template<typename T>
struct is_fixed : std::false_type {};
template<size_t Size>
struct is_fixed<fixed<Size>> : std::true_type {};
template<typename T>
constexpr bool is_fixed_v = is_fixed<T>::value;

template<typename Marker>
struct memory_unit {
  using marker = Marker;
};

template<typename T>
struct is_memory_unit : std::false_type {};
template<typename T>
struct is_memory_unit<memory_unit<T>> : std::true_type {};
template<typename T>
constexpr bool is_stm_v = is_memory_unit<T>::value;

template<typename T>
using marker_t = typename T::marker;
} // namespace size

template<typename Parser, typename Size>
struct lazy_sequence {
  template<typename ...Ts>
  struct ret {
    struct iterator {
      using iterator_category = std::input_iterator_tag;
      using difference_type = void;
      using value_type = target_t<Parser>;
      using pointer = const value_type*;
      using reference = const value_type&;
    private:
      std::istream *in_{};
      std::tuple<Ts...> *stm_{};
      bool end_{false};
    };

    iterator begin() {
      return iterator{in_, stm_};
    }

    iterator end() {
      return iterator{};
    }

  private:
    std::istream* in_;
    std::tuple<Ts...> *stm_;
  };

  template<typename... Ts>
  static ret<Ts...> parse(std::istream& in, std::tuple<Ts...>& stm) {
    return ret{&in, &stm};
  }
};

template<typename Parser, typename Size,
         typename Target = std::vector<target_t<Parser>>>
struct sequence {
  template<typename... Ts>
  static Target parse(std::istream& in, std::tuple<Ts...>& stm) {
    Target objs{};
    if constexpr (size::is_fixed_v<Size>) {
      objs.reserve(Size::value);
      for (int i = 0; i < Size::value; ++i)
        objs.emplace_back(Parser::parse(in, stm));
    } else if constexpr (size::is_stm_v<Size>) {
      const size_t size = std::get<Size>(stm);
      objs.reserve(size);
      for (int i = 0; i < size; ++i)
        objs.emplace_back(Parser::parse(in, stm));
    } else {
      target_t<Parser> obj;
      auto pos = in.tellg();
      obj = Parser::parse(in, stm);
      while (!in.fail()) {
        objs.emplace_back(obj);
        pos = in.tellg();
        obj = Parser::parse(in, stm);
      }
      in.seekg(pos);
      in.clear();
    }
    return objs;
  }
};

} // namespace parser
template<typename Parsers, typename STM = std::tuple<>>
class Parser {
public:
  using stm = STM;
  const stm& get_stm() const { return stm_; }

  auto parse(std::istream& in) { return Parsers::parse(in, stm_); }

private:
  stm stm_{};
};

} // namespace etl

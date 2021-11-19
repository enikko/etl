/**
 * @file trie.hpp
 *
 * @brief 
 *
 * @author Erik Nikko
 * Contact: 65210465+enikko@users.noreply..com
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

#include <map>
#include <tuple>
#include <vector>

#include "tm.hpp"
#include "type_traits.hpp"

namespace etl {

namespace trie {
struct word_existance {
  [[nodiscard]] bool exist() const { return exists_ == 1; }
  void increment_word() { exists_ = 1; }

private:
  int_fast8_t exists_{0};
};

template<typename Rep = unsigned>
struct word_count {
  [[nodiscard]] bool exist() const { return count_ > 0; }
  Rep count() const { return count_; }
  void increment_word() { ++count_; }
private:
  Rep count_{0};
};
} // namespace trie

template<typename Character = char,
         typename Accumulators = std::tuple<trie::word_count<>>>
class Trie {
private:
  template<typename T>
  static auto test_increment_word(int)
      -> decltype(std::declval<T>().increment_word(), std::true_type{});
  template<typename T>
  static auto test_increment_word(...) -> std::false_type;
  template<typename T>
  constexpr static bool has_increment_word_v =
      decltype(test_increment_word<T>(0))::value;

  struct node {
    Character c{};
    std::map<Character, size_t> children{};
    Accumulators accumulator{};
    void increment_word() {
      tm::for_each(accumulator, [](auto& acc) {
        if constexpr (has_increment_word_v<decltype(acc)>)
          acc.increment_word();
      });
    }
  };

public:
  template<typename SelectingAccumulator>
  class Iterator {
  private:
    struct state {
      size_t idx;
      std::optional<typename std::map<Character, size_t>::const_iterator> pos;
      bool operator==(const state &other) const {
        return idx == other.idx && pos == other.pos;
      }
      bool operator!=(const state &other) const {
        return !(*this == other);
      }
    };

  public:
    using iterator_category = std::input_iterator_tag;
    using difference_type = void;
    using value_type =
        std::pair<std::basic_string<Character>, const Accumulators*>;
    using pointer = const value_type*;
    using reference = const value_type&;

    Iterator() = default;
    explicit Iterator(const Trie& trie) : trie_{&trie} {
      search_queue_.emplace_back(
          state{0, std::make_optional(trie.nodes_[0].children.begin())});
      find_valid();
    }
    Iterator(const Iterator&) = default;
    Iterator(Iterator&&) noexcept = default;
    ~Iterator() = default;
    Iterator& operator=(const Iterator&) = default;
    Iterator& operator=(Iterator&&) noexcept = default;
    Iterator operator++(int) {
      Iterator orig{*this};
      ++*this;
      return orig;
    }
    Iterator& operator++() {
      if (!search_queue_.empty()) {
        search_queue_.back().pos =
            trie_->nodes_[search_queue_.back().idx].children.begin();
        find_valid();
      }
      return *this;
    }
    bool operator==(const Iterator& other) const {
      if (search_queue_.empty() && other.search_queue_.empty())
        return true;
      return trie_ == other.trie_ &&
             std::equal(search_queue_.begin(), search_queue_.end(),
                        other.search_queue_.begin(), other.search_queue_.end());
    }
    bool operator!=(const Iterator& other) const { return !(*this == other); }
    reference operator*() const { return current_value_; }
    pointer operator->() const { return &current_value_; }

  private:
    void find_valid() {
      while (!search_queue_.empty() &&
             search_queue_.back().pos ==
                 trie_->nodes_[search_queue_.back().idx].children.end()) {
        search_queue_.pop_back();
        current_value_.first.pop_back();
        if (search_queue_.empty())
          return;
        current_value_.second =
            &trie_->nodes_[search_queue_.back().idx].accumulator;
        ++*search_queue_.back().pos;
      }
      current_value_.first.push_back((*search_queue_.back().pos)->first);
      search_queue_.push_back(
          state{(*search_queue_.back().pos)->second, std::nullopt});
      current_value_.second =
          &trie_->nodes_[search_queue_.back().idx].accumulator;
      while (!std::get<SelectingAccumulator>(
                  trie_->nodes_[search_queue_.back().idx].accumulator)
                  .exist()) {
        search_queue_.back().pos =
            trie_->nodes_[search_queue_.back().idx].children.begin();
        current_value_.first.push_back((*search_queue_.back().pos)->first);
        search_queue_.push_back(
            state{(*search_queue_.back().pos)->second, std::nullopt});
        current_value_.second =
            &trie_->nodes_[search_queue_.back().idx].accumulator;
      }
    }
    value_type current_value_{};
    std::vector<state> search_queue_{};
    const Trie* trie_{};
  };

  template<typename Accumulator = std::tuple_element_t<0, Accumulators>>
  using const_iterator = Iterator<Accumulator>;

  template<typename Accumulator = std::tuple_element_t<0, Accumulators>>
  const_iterator<Accumulator> begin() const {
    return const_iterator<Accumulator>{*this};
  }

  template<typename Accumulator = std::tuple_element_t<0, Accumulators>>
  const_iterator<Accumulator> end() const {
    return const_iterator<Accumulator>{};
  }

  template<
      typename Iterator,
      std::enable_if_t<is_iterator_value_v<Iterator, Character>, bool> = true>
  void add(Iterator it, const Iterator end) {
    size_t parent = 0;
    for (; it != end; ++it) {
      const auto emplace_res =
          nodes_[parent].children.emplace(*it, nodes_.size());
      if (emplace_res.second) {
        nodes_.emplace_back();
        nodes_.back().c = *it;
      }
      parent = emplace_res.first->second;
    }
    nodes_[parent].increment_word();
  }

  template<
      typename T,
      std::enable_if_t<std::is_same_v<decltype(std::declval<Trie>().add(
                                          std::begin(std::declval<const T&>()),
                                          std::end(std::declval<const T&>()))),
                                      void>> = true>
  void add(const T& word) {
    add(std::begin(word), std::end(word));
  }

  void add(const std::basic_string<Character>& word) {
    add(std::begin(word), std::end(word));
  }

  template<
      typename Iterator,
      std::enable_if_t<is_iterator_value_v<Iterator, Character>, bool> = true>
  const Accumulators& get(Iterator it, const Iterator end) const {
    size_t parent = 0;
    for (; it != end; ++it) {
      const auto find_it = nodes_[parent].children.find(*it);
      if (find_it == nodes_[parent].children.end())
        return empty_accumulator;
      parent = find_it->second;
    }
    return nodes_[parent].accumulator;
  }

  template<
      typename T,
      std::enable_if_t<std::is_same_v<decltype(std::declval<Trie>().get(
                                          std::begin(std::declval<const T&>()),
                                          std::end(std::declval<const T&>()))),
                                      void>> = true>
  const Accumulators& get(const T& word) const {
    return get(std::begin(word), std::end(word));
  }

  const Accumulators& get(const std::basic_string<Character>& word) const {
    return get(std::begin(word), std::end(word));
  }

private:
  std::vector<node> nodes_{node{}};
  constexpr static Accumulators empty_accumulator{};
};

} // namespace etl

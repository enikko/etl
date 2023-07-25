/**
 * @file trie.hpp
 * This file contains an implementation of the trie data structure.
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

#include <map>
#include <optional>
#include <tuple>
#include <vector>

#include "type_traits.hpp"

namespace etl {

namespace trie {
/// @brief A word existence accumulator that simply marks if a word exists in the trie.
struct word_existence {

  /// @brief Checks if the word ending at the node exist in the trie.
  /// @brief Return true if the word exists.
  [[nodiscard]] bool exist() const { return exists_ == 1; }

  /// @brief Marks that the word exist in the trie.
  void increment_word() { exists_ = 1; }

private:
  int_fast8_t exists_{0};
};

/// @brief An accumulator that counts how many times a word has been added to the trie.
template<typename Rep = unsigned>
struct word_count {
  /// @brief Returns true if the word ending at the node has been added to the trie.
  /// @return True if the word has been added else false.
  [[nodiscard]] bool exist() const { return count_ > 0; }
  /// @brief Gets how many time the word has been added to the trie.
  /// @return The amount of times the word has been added to the trie.
  Rep count() const { return count_; }
  /// @brief Increments the amount of times the word has been added to the trie.
  void increment_word() { ++count_; }

private:
  Rep count_{0};
};
} // namespace trie

/// @brief An implementation of a trie with selectable policy for what is stored accumulated
///        in the nodes. Two of the most simple cases are the trie::word_existence and
///        trie::word_count accumulators. These allows for only computing if a word has been
///        added to the trie and the number of times a word has been added to the trie, respectively.
/// @tparam Character The elementary type of the elements in a word. Defaults to char.
/// @tparam Accumulators A std::tuple with the accumulators that are to be used in the trie.
template<typename Character = char,
         typename Accumulators = std::tuple<trie::word_count<>>>
class Trie {
private:
  /// @brief Checks whether the type @p T (assumed to be an accumulator)
  ///        has defined the method increment_word().
  template<typename T>
  static auto test_increment_word(int)
      -> decltype(std::declval<T>().increment_word(), std::true_type{});
  template<typename T>
  static auto test_increment_word(...) -> std::false_type;
  template<typename T>
  constexpr static bool has_increment_word_v =
      decltype(test_increment_word<T>(0))::value;

  /// @brief A node in the trie.
  struct node {
    Character c{};
    std::map<Character, size_t> children{};
    Accumulators accumulator{};
    /// @brief Increment the word ending in this node (i.e. this word was added to the trie).
    void increment_word() {
      std::apply(
          [](auto&... accs) {
            (
                [](auto& acc) -> void {
                  if constexpr (has_increment_word_v<decltype(acc)>)
                    acc.increment_word();
                }(accs),
                ...);
          },
          accumulator);
    }
  };

public:
  /// @brief An iterator that traverses the trie in a pre-order fashion.
  /// @tparam SelectingAccumulator The accumulator that is used in order to
  ///     decide if a node is a word added to the trie.
  template<typename SelectingAccumulator>
  class PreOrderIterator {
  private:
    /// @brief A state in the traversal is decided by the nodes index and how many child nodes
    ///     that has been traversed.
    struct state {
      size_t idx;
      std::optional<typename std::map<Character, size_t>::const_iterator> pos;
      bool operator==(const state& other) const {
        return idx == other.idx && pos == other.pos;
      }
      bool operator!=(const state& other) const { return !(*this == other); }
    };

  public:
    using iterator_category = std::input_iterator_tag;
    using difference_type = void;
    using value_type =
        std::pair<std::basic_string<Character>, const Accumulators*>;
    using pointer = const value_type*;
    using reference = const value_type&;

    /// @brief The default constructor creates and end interator
    PreOrderIterator() = default;
    /// @brief Creates a new begin operator for the provided trie.
    /// @param[in] trie The trie that the iterator is for.
    explicit PreOrderIterator(const Trie& trie) : trie_{&trie} {
      search_queue_.emplace_back(
          state{0, std::make_optional(trie.nodes_[0].children.begin())});
      find_valid();
    }
    PreOrderIterator(const PreOrderIterator&) = default;
    PreOrderIterator(PreOrderIterator&&) noexcept = default;
    ~PreOrderIterator() = default;
    PreOrderIterator& operator=(const PreOrderIterator&) = default;
    PreOrderIterator& operator=(PreOrderIterator&&) noexcept = default;

    /// @brief Moves the iterator to the next entry in the trie.
    PreOrderIterator operator++(int) {
      PreOrderIterator orig{*this};
      ++*this;
      return orig;
    }
    /// @copydoc operator++(int)
    PreOrderIterator& operator++() {
      if (!search_queue_.empty()) {
        search_queue_.back().pos =
            trie_->nodes_[search_queue_.back().idx].children.begin();
        find_valid();
      }
      return *this;
    }
    bool operator==(const PreOrderIterator& other) const {
      if (search_queue_.empty() && other.search_queue_.empty())
        return true;
      return trie_ == other.trie_ &&
             std::equal(search_queue_.begin(), search_queue_.end(),
                        other.search_queue_.begin(), other.search_queue_.end());
    }
    bool operator!=(const PreOrderIterator& other) const {
      return !(*this == other);
    }

    /// @brief Access the value the iterator points to.
    /// @return Const reference to the value.
    reference operator*() const { return current_value_; }

    /// @brief Access the value the iterator points to.
    /// @return Const pointer to the value.
    pointer operator->() const { return &current_value_; }

  private:
    /// @brief Finds the next node in the trie that is selectable according to SelectingAccumulator.
    ///        Note that the next node might be the current node, i.e. calling this method multiple times
    ///        without incrementing yields the same node.
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

  /// @brief Const iterator to the trie.
  /// @tparam Accumulator The accumulator that is used to select which nodes
  ///         that should be visited (using the Accumulator::exist method).
  ///         Defaults to the first type in Accumulators.
  /// @pre @p Accumulator is the same type as one of the types in Accumulators.
  template<typename Accumulator = std::tuple_element_t<0, Accumulators>>
  using const_iterator = PreOrderIterator<Accumulator>;

  /// @brief Gets a begin const iterator to the trie.
  /// @tparam Accumulator The accumulator that is used to select which nodes
  ///         that should be visited (using the Accumulator::exist method).
  ///         Defaults to the first type in Accumulators.
  /// @pre @p Accumulator is the same type as one of the types in Accumulators.
  /// @return Const begin iterator.
  template<typename Accumulator = std::tuple_element_t<0, Accumulators>>
  const_iterator<Accumulator> begin() const {
    return const_iterator<Accumulator>{*this};
  }

  /// @brief Gets a begin const iterator to the trie.
  /// @tparam Accumulator The accumulator that is used to select which nodes
  ///         that should be visited (using the Accumulator::exist method).
  ///         Defaults to the first type in Accumulators.
  /// @pre @p Accumulator is the same type as one of the types in Accumulators.
  /// @return Const end iterator.
  template<typename Accumulator = std::tuple_element_t<0, Accumulators>>
  const_iterator<Accumulator> end() const {
    return const_iterator<Accumulator>{};
  }

  /// @brief Adds the word [@p it, @p end[ to the trie.
  /// @param[in] begin The begin iterator to the word.
  /// @param[in] end The end iterator to the word.
  /// @pre @p begin and @p end is a valid range.
  template<
      typename Iterator,
      std::enable_if_t<is_iterator_value_v<Iterator, Character>, bool> = true>
  void add(Iterator begin, const Iterator end) {
    size_t parent = 0;
    for (; begin != end; ++begin) {
      const auto emplace_res =
          nodes_[parent].children.emplace(*begin, nodes_.size());
      if (emplace_res.second) {
        nodes_.emplace_back();
        nodes_.back().c = *begin;
      }
      parent = emplace_res.first->second;
    }
    nodes_[parent].increment_word();
  }

  /// @brief Adds the word @p word to the trie.
  /// @param[in] word The word that will be added.
  /// @pre @p word has begin and end iterators defined.
  template<
      typename T,
      std::enable_if_t<std::is_same_v<decltype(std::declval<Trie>().add(
                                          std::begin(std::declval<const T&>()),
                                          std::end(std::declval<const T&>()))),
                                      void>> = true>
  void add(const T& word) {
    add(std::begin(word), std::end(word));
  }

  /// @brief Adds the word @p word to the trie.
  /// @param[in] word The word that will be added.
  void add(const std::basic_string<Character>& word) {
    add(std::begin(word), std::end(word));
  }

  /// @brief Gets the accumulators for the word [@p begin, @p end[.
  /// @param[in] begin The begin iterator to the word.
  /// @param[in] end The end iterator to the word.
  /// @pre @p begin and @p end is a valid range.
  /// @return The accumulator for the word if the trie contains the word
  ///         else a default constructed accumulator.
  template<
      typename Iterator,
      std::enable_if_t<is_iterator_value_v<Iterator, Character>, bool> = true>
  const Accumulators& get(Iterator begin, const Iterator end) const {
    size_t parent = 0;
    for (; begin != end; ++begin) {
      const auto find_it = nodes_[parent].children.find(*begin);
      if (find_it == nodes_[parent].children.end())
        return empty_accumulator;
      parent = find_it->second;
    }
    return nodes_[parent].accumulator;
  }

  /// @brief Gets the accumulators for @p word.
  /// @param[in] word The word that is looked up in the trie.
  /// @pre std::begin and std::end are defined for @p word.
  /// @return The accumulator for the word if the trie contains the word
  ///         else a default constructed accumulator.
  template<
      typename T,
      std::enable_if_t<std::is_same_v<decltype(std::declval<Trie>().get(
                                          std::begin(std::declval<const T&>()),
                                          std::end(std::declval<const T&>()))),
                                      void>> = true>
  const Accumulators& get(const T& word) const {
    return get(std::begin(word), std::end(word));
  }

  /// @brief Gets the accumulators for @p word.
  /// @param[in] word The word that is looked up in the trie.
  /// @return The accumulator for the word if the trie contains the word
  ///         else a default constructed accumulator.
  const Accumulators& get(const std::basic_string<Character>& word) const {
    return get(std::begin(word), std::end(word));
  }

private:
  std::vector<node> nodes_{node{}};
  constexpr static Accumulators empty_accumulator{};
};

} // namespace etl

/**
 * @file This file contains the test suite for the trie implementation.
 *
 * @author Erik Nikko
 * Contact: 65210465+enikko@users.noreply.github.com
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

#include <cassert>

#include "etl/trie.hpp"

int main() {
  {
    etl::Trie<> trie{};
    trie.add("foo");
    trie.add("bar");
    trie.add("foo");
    assert(std::get<etl::trie::word_count<>>(trie.get("foo")).count() == 2);
    assert(std::get<etl::trie::word_count<>>(trie.get("bar")).count() == 1);
    assert(std::get<etl::trie::word_count<>>(trie.get("foobar")).count() == 0);
  }
  {
    etl::Trie<char, std::tuple<etl::trie::word_existence>> trie{};
    trie.add("foo");
    trie.add("bar");
    trie.add("foo");
    assert(std::get<etl::trie::word_existence>(trie.get("foo")).exist());
    assert(std::get<etl::trie::word_existence>(trie.get("bar")).exist());
    assert(!std::get<etl::trie::word_existence>(trie.get("foobar")).exist());
  }
  {
    etl::Trie<char,
              std::tuple<etl::trie::word_existence, etl::trie::word_count<>>>
        trie{};
    trie.add("foo");
    trie.add("bar");
    trie.add("foo");
    assert(std::get<etl::trie::word_count<>>(trie.get("foo")).exist());
    assert(std::get<etl::trie::word_count<>>(trie.get("bar")).exist());
    assert(!std::get<etl::trie::word_count<>>(trie.get("foobar")).exist());
    assert(std::begin(trie)->first == "bar");
    assert(
        std::get<etl::trie::word_count<>>(*std::begin(trie)->second).count() ==
        1);
  }
  return 0;
}

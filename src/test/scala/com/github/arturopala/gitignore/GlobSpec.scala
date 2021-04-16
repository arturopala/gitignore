/*
 * Copyright 2020 Artur Opala
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.arturopala.gitignore

class GlobSpec extends AnyWordSpecCompat {

  Debug.isDebug = false

  "Glob.Matcher" should {
    "compute match contour" in {
      val z1 = Zoom("abbc")
      Glob.Matcher.computeContour(z1, Glob.compile("b")) === true
      z1.contour === (1, 3)
      Glob.Matcher.computeContour(z1, Glob.compile("a")) === true
      z1.contour === (0, 1)
      Glob.Matcher.computeContour(z1, Glob.compile("c")) === true
      z1.contour === (3, 4)
      Glob.Matcher.computeContour(z1, Glob.compile("d")) === false
      z1.hasContour === false
      Glob.Matcher.computeContour(z1, Glob.compile("?")) === true
      z1.contour === (0, 4)
      Glob.Matcher.computeContour(z1, Glob.compile("*")) === true
      z1.contour === (0, 4)
      Glob.Matcher.computeContour(z1, Glob.compile("**")) === true
      z1.contour === (0, 4)
      Glob.Matcher.computeContour(z1, Glob.compile("*b*")) === true
      z1.contour === (0, 4)
      Glob.Matcher.computeContour(z1, Glob.compile("*??*")) === true
      z1.contour === (0, 4)
      Glob.Matcher.computeContour(z1, Glob.compile("*?b*")) === true
      z1.contour === (0, 4)
      Glob.Matcher.computeContour(z1, Glob.compile("*?c")) === true
      z1.contour === (0, 4)
      Glob.Matcher.computeContour(z1, Glob.compile("*?d")) === false
      Glob.Matcher.computeContour(z1, Glob.compile("d")) === false
      Glob.Matcher.computeContour(z1, Glob.compile("?????")) === false
    }
  }

  "Glob" should {
    "check if pattern has wildcards" in {
      Glob.isWildcardPattern("a") === false
      Glob.isWildcardPattern("abc") === false
      Glob.isWildcardPattern("]") === false
      Glob.isWildcardPattern("") === false

      Glob.isWildcardPattern("ab?c") === true
      Glob.isWildcardPattern("[") === true
      Glob.isWildcardPattern("[]") === true
      Glob.isWildcardPattern("[a]") === true
      Glob.isWildcardPattern("[a-z]") === true
      Glob.isWildcardPattern("*") === true
      Glob.isWildcardPattern("**") === true
      Glob.isWildcardPattern("?") === true
      Glob.isWildcardPattern("a*") === true
      Glob.isWildcardPattern("a/**/b") === true
      Glob.isWildcardPattern("?a") === true
    }

    "reject an empty pattern" in {
      an[Exception] shouldBe thrownBy {
        Glob.compile("").matcher("a")
      }
      an[Exception] shouldBe thrownBy {
        Glob.compile("").matcher("")
      }
    }

    "compile the pattern " in {
      Glob.compile("a") === Glob.LiteralPattern("a")
      Glob.compile("?") === Glob.AnySingleCharacterPattern
      Glob.compile("\\?") === Glob.LiteralPattern("?")
      Glob.compile("*") === Glob.AnyStringPattern
      Glob.compile("\\*") === Glob.LiteralPattern("*")
      Glob.compile("**") === Glob.AnythingPattern
      Glob.compile("[]") === Glob.LiteralPattern("[]")
      Glob.compile("[a]") === Glob.BracketPattern("a")
      Glob.compile("[abc]") === Glob.BracketPattern("abc")
      Glob.compile("[a-z]") === Glob.BracketPattern("a-z")
      Glob.compile("[!a-z]") === Glob.BracketPattern("!a-z")
      Glob.compile("[!abc]") === Glob.BracketPattern("!abc")
      Glob.compile("[[]") === Glob.BracketPattern("[")
      Glob.compile("[-]") === Glob.BracketPattern("-")
      Glob.compile("[-a]") === Glob.BracketPattern("-a")
      Glob.compile("[a-]") === Glob.BracketPattern("a-")
      Glob.compile("[a\\]") === Glob.LiteralPattern("[a]")
    }

    "compile literal pattern and match the string" in {
      val m1 = Glob.compile("a").matcher("a")
      m1.find() === true
      m1.start() === 0
      m1.end() === 1

      val m2 = Glob.compile("a").matcher("aaa")
      m2.find() === true
      m2.start() === 0
      m2.end() === 1

      val m3 = Glob.compile("abc").matcher("abc")
      m3.find() === true
      m3.start() === 0
      m3.end() === 3

      val m4 = Glob.compile("abc").matcher("abcabc")
      m4.find() === true
      m4.start() === 0
      m4.end() === 3

      val m5 = Glob.compile("abc").matcher("aabcc")
      m5.find() === true
      m5.start() === 1
      m5.end() === 4

      val m6 = Glob.compile("abc").matcher("aabbcc")
      m6.find() === false
      m6.start() === -1
      m6.end() === -1

      val m7 = Glob.compile("abb").matcher("abc")
      m7.find() === false
      m7.start() === -1
      m7.end() === -1

      Glob.compile("abb").matcher("abb").find() === true
      Glob.compile("abb").matcher("abbb").find() === true
      Glob.compile("abb").matcher("aabb").find() === true
      Glob.compile("abb").matcher("aabbb").find() === true

      Glob.compile("abc").matcher("").find() === false
      Glob.compile("abc").matcher("abbc").find() === false
      Glob.compile("abc").matcher("ab").find() === false
      Glob.compile("abc").matcher("a").find() === false
      Glob.compile("abc").matcher("ac").find() === false
      Glob.compile("abc").matcher("bc").find() === false
      Glob.compile("abc").matcher("bcc").find() === false
      Glob.compile("abc").matcher("accc").find() === false
      Glob.compile("abc").matcher("cba").find() === false
      Glob.compile("abc").matcher("aabb").find() === false

      Glob.compile("/abc/").matcher("/aba/abc/abd/").find() === true
    }

    "compile single ? wildcard pattern and match the string" in {
      val m1 = Glob.compile("?").matcher("a")
      m1.find() === true
      m1.start() === 0
      m1.end() === 1

      val m2 = Glob.compile("?").matcher("abc")
      m2.find() === true
      m2.start() === 0
      m2.end() === 1

      val m3 = Glob.compile("?").matcher("")
      m3.find() === false

      val p1 = Glob.compile("?")
      p1.matcher("b").find() === true
      p1.matcher("ab").find() === true
      p1.matcher("bb").find() === true
      p1.matcher("zzz").find() === true
      p1.matcher(" ").find() === true
      p1.matcher("").find() === false

      val p2 = Glob.compile("\\??")
      p2.matcher("a").find() === false
      p2.matcher("?").find() === false
      p2.matcher("?a").find() === true
      p2.matcher("??").find() === true
      p2.matcher("?b").find() === true
      p2.matcher("?ab").find() === true
      p2.matcher("????").find() === true
    }

    "compile single * wildcard pattern and match the string" in {
      val m1 = Glob.compile("*").matcher("a")
      m1.find() === true
      m1.start() === 0
      m1.end() === 1

      val m2 = Glob.compile("*").matcher("abc")
      m2.find() === true
      m2.start() === 0
      m2.end() === 3

      val m3 = Glob.compile("*").matcher("")
      m3.find() === true
      m3.start() === 0
      m3.end() === 0

      val p1 = Glob.compile("*")
      p1.matcher("b").find() === true
      p1.matcher("bb").find() === true
      p1.matcher("bbb").find() === true
      p1.matcher("bbbb").find() === true
      p1.matcher(" ").find() === true
      p1.matcher("").find() === true

      val p2 = Glob.compile("\\*")
      p2.matcher("b").find() === false
      p2.matcher("bb").find() === false
      p2.matcher("b*b").find() === true
      p2.matcher("bbb*").find() === true
      p2.matcher("* ").find() === true
      p2.matcher("*").find() === true
      p2.matcher("*b*").find() === true
      p2.matcher("***").find() === true
    }

    "compile mixed ? pattern and match the string" in {
      val m1 = Glob.compile("a?").matcher("ab")
      m1.find() === true
      m1.start() === 0
      m1.end() === 2

      val m2 = Glob.compile("?b").matcher("ab")
      m2.find() === true
      m2.start() === 0
      m2.end() === 2

      val m3 = Glob.compile("a?c").matcher("abc")
      m3.find() === true
      m3.start() === 0
      m3.end() === 3

      val m4 = Glob.compile("a?b").matcher("abc")
      m4.find() === false

      val m5 = Glob.compile("?a").matcher("ab")
      m5.find() === false

      Glob.compile("a?a").matcher("aaa").find() === true

      val p1 = Glob.compile("??")
      p1.matcher("").find() === false
      p1.matcher(" ").find() === false
      p1.matcher("a").find() === false
      p1.matcher("a ").find() === true
      p1.matcher("aa").find() === true
      p1.matcher("aaa").find() === true
      p1.matcher("abc").find() === true
      p1.matcher("abb").find() === true
      p1.matcher("abbc").find() === true
      p1.matcher("abbc").find() === true

      val p2 = Glob.compile("???")
      p2.matcher("").find() === false
      p2.matcher(" ").find() === false
      p2.matcher("a").find() === false
      p2.matcher("a ").find() === false
      p2.matcher("aa").find() === false
      p2.matcher("aaa").find() === true
      p2.matcher("abc").find() === true
      p2.matcher("abb").find() === true
      p2.matcher("abbc").find() === true
      p2.matcher("abbc").find() === true

      val p3 = Glob.compile("?b?")
      p3.matcher("").find() === false
      p3.matcher(" ").find() === false
      p3.matcher("ab").find() === false
      p3.matcher("abc").find() === true
      p3.matcher("abbc").find() === true
      p3.matcher("bb").find() === false
      p3.matcher("bb ").find() === true
      p3.matcher(" b ").find() === true
      p3.matcher("b b").find() === false
      p3.matcher(" b b").find() === true
      p3.matcher(" b a").find() === true
      p3.matcher("  ba").find() === true
      p3.matcher("  ba ").find() === true

      val p4 = Glob.compile("a?c?")
      p4.matcher("").find() === false
      p4.matcher(" ").find() === false
      p4.matcher("ab").find() === false
      p4.matcher("abc").find() === false
      p4.matcher("abcd").find() === true
      p4.matcher("aBcd").find() === true
      p4.matcher("aBCd").find() === false
      p4.matcher("dabc").find() === false
      p4.matcher("dabcd").find() === true
      p4.matcher("abc ").find() === true
      p4.matcher("abbc").find() === false
      p4.matcher("abcc").find() === true
      p4.matcher("ab").find() === false
      p4.matcher("ab ").find() === false
      p4.matcher("ab c").find() === false

      val p5 = Glob.compile("?abc?")
      p5.matcher("").find() === false
      p5.matcher(" ").find() === false
      p5.matcher("ab").find() === false
      p5.matcher("abc").find() === false
      p5.matcher("abcd").find() === false
      p5.matcher("dabc").find() === false
      p5.matcher("dabcd").find() === true
      p5.matcher("abc ").find() === false
      p5.matcher("abbc").find() === false
      p5.matcher("abcc").find() === false
      p5.matcher("ab").find() === false
      p5.matcher("ab ").find() === false
      p5.matcher("ab c").find() === false
      p5.matcher(" abc ").find() === true
      p5.matcher("abcabcabc").find() === true
      p5.matcher("abcABCabc").find() === false
    }

    "compile mixed literal and * pattern, and match the string" in {
      val m1 = Glob.compile("a*").matcher("ab")
      m1.find() === true
      m1.start() === 0
      m1.end() === 2

      val m2 = Glob.compile("a*").matcher("abab")
      m2.find() === true
      m2.start() === 0
      m2.end() === 4

      val m3 = Glob.compile("c*").matcher("abab")
      m3.find() === false
      m3.start() === -1
      m3.end() === -1

      val m4 = Glob.compile("b*").matcher("abab")
      m4.find() === true
      m4.start() === 1
      m4.end() === 4

      val m5 = Glob.compile("ab*").matcher("abab")
      m5.find() === true
      m5.start() === 0
      m5.end() === 4

      val m6 = Glob.compile("ba*").matcher("abab")
      m6.find() === true
      m6.start() === 1
      m6.end() === 4

      val p1 = Glob.compile("*a")
      p1.matcher("b").find() === false
      p1.matcher("bb").find() === false
      p1.matcher("bbb").find() === false

      val p2 = Glob.compile("\\*a")
      p2.matcher("*b").find() === false
      p2.matcher("b*b").find() === false
      p2.matcher("bb*b").find() === false
      p2.matcher("bb*a").find() === true
      p2.matcher("b*ab").find() === true

      val p3 = Glob.compile("\\*?")
      p3.matcher("*b").find() === true
      p3.matcher("b*b").find() === true
      p3.matcher("bb*b").find() === true
      p3.matcher("bb*a").find() === true
      p3.matcher("b*ab").find() === true
      p3.matcher("bab*").find() === false
      p3.matcher("baba").find() === false
    }

    "compile mixed * and literal pattern, and match the string" in {
      val m1 = Glob.compile("*a").matcher("ab")
      m1.find() === true
      m1.start() === 0
      m1.end() === 1

      val m2 = Glob.compile("*a*").matcher("ab")
      m2.find() === true
      m2.start() === 0
      m2.end() === 2

      val p1 = Glob.compile("*a*b")
      val m3 = p1.matcher("abab")
      m3.find() === true
      m3.start() === 0
      m3.end() === 4

      val m4 = p1.matcher("abc")
      m4.find() === true
      m4.start() === 0
      m4.end() === 2

      val m5 = p1.matcher("abbb")
      m5.find() === true
      m5.start() === 0
      m5.end() === 4

      val p2 = Glob.compile("*a*b*")
      val m6 = p2.matcher("abab")
      m6.find() === true
      m6.start() === 0
      m6.end() === 4

      val m7 = p2.matcher("abc")
      m7.find() === true
      m7.start() === 0
      m7.end() === 3

      val m8 = p2.matcher("babababab")
      m8.find() === true
      m8.start() === 0
      m8.end() === 9

      val p3 = Glob.compile("*ab*cd*")
      val m9 = p3.matcher("abcd")
      m9.find() === true
      m9.start() === 0
      m9.end() === 4

      val m10 = p3.matcher("ababcdcd")
      m10.find() === true
      m10.start() === 0
      m10.end() === 8

      val m11 = p3.matcher("cdabcdefghijkcdab")
      m11.find() === true
      m11.start() === 0
      m11.end() === 17

      val p4 = Glob.compile("ab*c*d")
      val m12 = p4.matcher("abcd")
      m12.find() === true
      m12.start() === 0
      m12.end() === 4

      val m13 = p4.matcher("eababcdcede")
      m13.find() === true
      m13.start() === 1
      m13.end() === 10

      val m14 = p4.matcher("cdabcdefghijkceedab")
      m14.find() === true
      m14.start() === 2
      m14.end() === 17
    }

    "compile mixed ? and * and literal pattern, and match the string" in {
      val p1 = Glob.compile("/*abc?/")
      val m1 = p1.matcher("/.abc/")
      m1.find() === false
    }

    "compile path wildcard ** and match the string" in {
      val m1 = Glob.compile("**").matcher("a/b")
      m1.find() === true
      m1.start() === 0
      m1.end() === 3

      val m2 = Glob.compile("**/**").matcher("a/b")
      m2.find() === true
      m2.start() === 0
      m2.end() === 3

      val m3 = Glob.compile("**/**").matcher("abc/aba")
      m3.find() === true
      m3.start() === 0
      m3.end() === 7

      val m4 = Glob.compile("b?/**").matcher("abc/aba")
      m4.find() === true
      m4.start() === 1
      m4.end() === 7

      val m5 = Glob.compile("b?/**/?d").matcher("abc/ef/ghij/kl/cd")
      m5.find() === true
      m5.start() === 1
      m5.end() === 17

      val m6 = Glob.compile("/ab?/**/?d/").matcher("/abc/ef/ghij/kl/cd/")
      m6.find() === true
      m6.start() === 0
      m6.end() === 19

      Glob.compile("/ab?/**/?c/").matcher("/abc/ef/ghij/kl/cd/").find() === false
      Glob.compile("/ba?/**/?d/").matcher("/abc/ef/ghij/kl/cd/").find() === false
      Glob.compile("/ab?/**l/?d/").matcher("/abc/ef/ghij/kl/cd/").find() === true
      Glob.compile("/ab?/**k/?d/").matcher("/abc/ef/ghij/kl/cd/").find() === false
    }

    "reject an empty character pattern" in {
      an[Exception] shouldBe thrownBy {
        Glob.CharacterCheck.compile("")
      }
    }

    "compile character class check" in {
      val ch1 = Glob.CharacterCheck.compile("abcd")
      ch1.check('a') === true
      ch1.check('b') === true
      ch1.check('c') === true
      ch1.check('d') === true
      ch1.check('e') === false
      ch1.check('f') === false
      ch1.check('0') === false
      ch1.check('9') === false
      ch1.check('A') === false
      ch1.check('B') === false
      ch1.check('C') === false
      ch1.check('D') === false
      ch1.check('E') === false

      Glob.CharacterCheck.compile("[").check('[') === true
      Glob.CharacterCheck.compile("]").check(']') === true
      Glob.CharacterCheck.compile("]").check('[') === false
      Glob.CharacterCheck.compile("[").check(']') === false
      Glob.CharacterCheck.compile("?").check('?') === true
      Glob.CharacterCheck.compile("?").check('a') === false
      Glob.CharacterCheck.compile("*").check('*') === true
      Glob.CharacterCheck.compile("*").check('a') === false
      Glob.CharacterCheck.compile("*").check('?') === false
      Glob.CharacterCheck.compile("?").check('*') === false

      Glob.CharacterCheck.compile("-?*[]-").check('*') === true
      Glob.CharacterCheck.compile("-?*[]-").check('-') === true
      Glob.CharacterCheck.compile("-?*[]-").check('?') === true
      Glob.CharacterCheck.compile("-?*[]-").check('[') === true
      Glob.CharacterCheck.compile("-?*[]-").check(']') === true
      Glob.CharacterCheck.compile("-?*[]a-").check('a') === true
    }

    "compile combined character class and range check" in {
      val ch1 = Glob.CharacterCheck.compile("ac-fz-")
      ch1.check('a') === true
      ch1.check('b') === false
      ch1.check('c') === true
      ch1.check('d') === true
      ch1.check('e') === true
      ch1.check('f') === true
      ch1.check('g') === false
      ch1.check('h') === false
      ch1.check('z') === true
      ch1.check('-') === true
      ch1.check('.') === false
    }

    "reject charecter class containg explict /" in {
      an[Exception] shouldBe thrownBy {
        Glob.CharacterCheck.compile("-?*[]a/-")
      }
    }

    "compile character range check" in {
      val ch1 = Glob.CharacterCheck.compile("a-fB-D1-6+.")
      ch1.check('a') === true
      ch1.check('b') === true
      ch1.check('c') === true
      ch1.check('d') === true
      ch1.check('e') === true
      ch1.check('f') === true
      ch1.check('g') === false
      ch1.check('h') === false
      ch1.check('z') === false

      ch1.check('A') === false
      ch1.check('B') === true
      ch1.check('C') === true
      ch1.check('D') === true
      ch1.check('E') === false
      ch1.check('F') === false
      ch1.check('G') === false
      ch1.check('H') === false
      ch1.check('Z') === false

      ch1.check('0') === false
      ch1.check('1') === true
      ch1.check('2') === true
      ch1.check('3') === true
      ch1.check('4') === true
      ch1.check('5') === true
      ch1.check('6') === true
      ch1.check('7') === false
      ch1.check('8') === false
      ch1.check('9') === false

      ch1.check('-') === false
      ch1.check('+') === true
      ch1.check('/') === false
      ch1.check('?') === false
      ch1.check('*') === false
      ch1.check('.') === true

      Glob.CharacterCheck.compile("\"'\\!a-z").check('\'') === true
      Glob.CharacterCheck.compile("\"'\\!a-z").check('"') === true
      Glob.CharacterCheck.compile("\"'\\!a-z").check('\\') === true
      Glob.CharacterCheck.compile("\"'\\!a-z").check('-') === false
      Glob.CharacterCheck.compile("\"'\\!a-z").check('A') === false
      Glob.CharacterCheck.compile("\"'\\!a-z").check('z') === true
      Glob.CharacterCheck.compile("\"'\\!a-z").check('!') === true
      Glob.CharacterCheck.compile("\"'\\!a-z").check('/') === false
    }

    "compile character check negation" in {
      Glob.CharacterCheck.compile("!a").check('a') === false
      Glob.CharacterCheck.compile("!a").check('b') === true
      Glob.CharacterCheck.compile("!a").check('/') === false
      Glob.CharacterCheck.compile("!\"'\\!a-z").check('\'') !== true
      Glob.CharacterCheck.compile("!\"'\\!a-z").check('"') !== true
      Glob.CharacterCheck.compile("!\"'\\!a-z").check('\\') !== true
      Glob.CharacterCheck.compile("!\"'\\!a-z").check('-') !== false
      Glob.CharacterCheck.compile("!\"'\\!a-z").check('A') !== false
      Glob.CharacterCheck.compile("!\"'\\!a-z").check('z') !== true
      Glob.CharacterCheck.compile("!\"'\\!a-z").check('!') !== true
      Glob.CharacterCheck.compile("!\"'\\!a-z").check('/') === false
    }

    "compile wildcard range pattern and match the string" in {
      Glob.compile("[]").matcher("[]").find() === true
      Glob.compile("[]").matcher(" ").find() === false
      Glob.compile("[]").matcher("").find() === false
      Glob.compile("[]]").matcher("]").find() === true
      Glob.compile("[]-]").matcher("]").find() === true
      Glob.compile("[]-]").matcher("-").find() === true
      Glob.compile("[]-]").matcher("[").find() === false
      Glob.compile("[][-]").matcher("[").find() === true
      Glob.compile("[a]").matcher("a").find() === true
      Glob.compile("[a]").matcher("bac").find() === true
      Glob.compile("[a-z]").matcher("a").find() === true
      Glob.compile("[a-zA-Z]").matcher("a").find() === true
      Glob.compile("[!b]").matcher("a").find() === true
      Glob.compile("[!b]").matcher("abcdef").find() === true
      Glob.compile("[!b-z]").matcher("a").find() === true
      Glob.compile("[!b-zA-Z]").matcher("a").find() === true
      Glob.compile("[b]").matcher("a").find() === false
      Glob.compile("[b-z]").matcher("a").find() === false
      Glob.compile("[--0]").matcher("-").find() === true
      Glob.compile("[--0]").matcher(".").find() === true
      Glob.compile("[--0]").matcher("0").find() === true
      Glob.compile("[--0]").matcher("+").find() === false
      Glob.compile("[[?*\\\\]").matcher("?").find() === true
      Glob.compile("[[?*\\\\]").matcher("*").find() === true
      Glob.compile("[[?*\\\\]").matcher("\\").find() === true
      Glob.compile("[[?*\\\\]").matcher("[").find() === true
    }

    "not match path separator / with ? and *" in {
      Glob.compile("?").matcher("/").find() === false
      Glob.compile("a?b").matcher("a/b").find() === false
      Glob.compile("a/b").matcher("a/b").find() === true
      Glob.compile("a?/?b").matcher("ac/db").find() === true
      Glob.compile("*").matcher("/").find() === true
      Glob.compile("?").matcher("/").find() === false
      Glob.compile("/").matcher("/").find() === true
      Glob.compile("a*b").matcher("ac/db").find() === false
      Glob.compile("a*/*b").matcher("ac/db").find() === true
      Glob.compile("a/*b").matcher("a/cdb").find() === true
      Glob.compile("a*/b").matcher("acd/b").find() === true
      Glob.compile("a*/?/e").matcher("acd/b/e").find() === true
      Glob.compile("a*/?/e").matcher("acd/b/f").find() === false
      Glob.compile("a*/?/e").matcher("acd/bf/e").find() === false
      Glob.compile("a*/??/e").matcher("acd/bf/e").find() === true
      Glob.compile("a*/???/e").matcher("acd/bf/e").find() === false
      Glob.compile("a*/*/e").matcher("acd/bf/e").find() === true
      Glob.compile("/ab/*/*/*/ba/").matcher("/ab/cd/efg/h_ijg/ba/").find() === true
      Glob.compile("/ab/*/*/?_*/ba/").matcher("/ab/cd/efg/h_ijg/ba/").find() === true
      Glob.compile("/ab/*/*/?_*/ba/").matcher("/ab/cd/efg/hijg/ba/").find() === false
      Glob.compile("/ab/*/*/?_?j*/ba/").matcher("/ab/cd/efg/h_ijg/ba/").find() === true
      Glob.compile("/ab/*/*/?_?j*/ba/").matcher("/ab/cd/efg/h-ijg/ba/").find() === false
    }

    "compile wildcard pattern and match the string recursively" in {
      Glob.compile("*").matcher("/a/b/").find() === true
      Glob.compile("/*").matcher("/a/b/").find() === true
      Glob.compile("*/").matcher("/a/b/").find() === true
      Glob.compile("*/*").matcher("/a/b/").find() === true
      Glob.compile("/*/*").matcher("/a/b/").find() === true
      Glob.compile("/*/").matcher("/a/b/").find() === true
      Glob.compile("/*/*/").matcher("/a/b/").find() === true
      Glob.compile("/**/").matcher("/a/b/").find() === true
      Glob.compile("/**").matcher("/a/b/").find() === true
      Glob.compile("**/").matcher("/a/b/").find() === true
      Glob.compile("**").matcher("/a/b/").find() === true
    }
  }

}

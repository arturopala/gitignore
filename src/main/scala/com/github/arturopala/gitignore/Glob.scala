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

import scala.annotation.tailrec

/** Globbing pathnames.
  *
  * Wildcard pattern matcher implementing the same rules as
  * https://www.man7.org/linux/man-pages/man7/glob.7.html
  *
  * Note that wildcard patterns are not regular expressions, although
  * they are a bit similar.  First of all, they match filenames,
  * rather than text, and secondly, the conventions are not the same:
  * for example, in a regular expression '*' means zero or more
  * copies of the preceding thing.
  *
  * Pattern syntax:
  *
  * A string is a wildcard pattern if it contains one of the
  * characters '?', '*' or '['.  Globbing is the operation that
  * expands a wildcard pattern into the list of pathnames matching
  * the pattern. Matching is defined by:
  *
  * A '?' (not between brackets) matches any single character.
  *
  * A '*' (not between brackets) matches any string, including the
  *       empty string.
  *
  * A '/' in a pathname cannot be matched by a '?' or '*' wildcard,
  *       or by a range like "[.-0]". A range containing an explicit
  *       '/' character is syntactically incorrect.
  *
  * An expression "[...]" where the first character after the leading
  * '[' is not an '!' matches a single character, namely any of the
  * characters enclosed by the brackets.  The string enclosed by the
  * brackets cannot be empty; therefore ']' can be allowed between
  * the brackets, provided that it is the first character.  (Thus,
  * "[][!]" matches the three characters '[', ']' and '!'.)
  *
  * There is one special convention: two characters separated by '-'
  * denote a range.  (Thus, "[A-Fa-f0-9]" is equivalent to
  * "[ABCDEFabcdef0123456789]".)  One may include '-' in its literal
  * meaning by making it the first or last character between the
  * brackets.  (Thus, "[]-]" matches just the two characters ']' and
  * '-', and "[--0]" matches the three characters '-', '.', '0',
  * since '/' cannot be matched.)
  *
  * Now that regular expressions have bracket expressions where the
  * negation is indicated by a '^', POSIX has declared the effect of
  * a wildcard pattern "[^...]" to be undefined.
  *
  * An expression "[!...]" matches a single character, namely any
  * character that is not matched by the expression obtained by
  * removing the first '!' from it.  (Thus, "[!]a-]" matches any
  * single character except ']', 'a' and '-'.)
  *
  * One can remove the special meaning of '?', '*' and '[' by
  * preceding them by a backslash, or, in case this is part of a
  * shell command line, enclosing them in quotes.  Between brackets
  * these characters stand for themselves.  Thus, "[[?*\]" matches
  * the four characters '[', '?', '*' and '\'.
  */
object Glob {

  val debug: Boolean = false

  final def isWildcardPattern(pattern: String): Boolean =
    pattern.foldLeft(false) { (a, c) =>
      a || (c match {
        case '?' | '*' | '[' => true
        case _               => false
      })
    }

  /** Compile pattern expression as re-usable [[Pattern]] instance. */
  final def compile(pattern: String): Pattern = {

    val (patterns, remaining, _, outsideBracket) = pattern
      .foldLeft((List.empty[Pattern], "", true, true)) { case ((ps, acc, notEscaped, outsideBracket), c) =>
        if (c == '?' && notEscaped && outsideBracket) {
          if (acc.isEmpty)
            (AnySingleCharacterPattern :: ps, "", true, true)
          else
            (AnySingleCharacterPattern :: LiteralPattern(acc) :: ps, "", true, true)
        } else if (c == '*' && notEscaped && outsideBracket) {
          if (acc.isEmpty)
            ps match {
              case AnyStringPattern :: ps2 =>
                (AnyPathPattern :: ps2, "", true, true)
              case _ =>
                (AnyStringPattern :: ps, "", true, true)
            }
          else
            (AnyStringPattern :: LiteralPattern(acc) :: ps, "", true, true)
        } else if (c == '[' && notEscaped && outsideBracket) {
          if (acc.isEmpty) (ps, "", true, false)
          else (LiteralPattern(acc) :: ps, "", true, false)
        } else if (c == ']' && notEscaped && !outsideBracket && acc.nonEmpty) {
          (new BracketPattern(acc) :: ps, "", true, true)
        } else {
          if (notEscaped)
            (ps, acc + c, c != '\\', outsideBracket)
          else
            (ps, acc.dropRight(1) + c, true, outsideBracket)
        }
      }

    val remaining2 =
      if (outsideBracket) remaining else "[" + remaining

    // add remaining string as a literal pattern
    val patterns2 = if (remaining2.nonEmpty) {
      patterns match {
        case Nil => LiteralPattern(remaining2) :: Nil
        case _   => LiteralPattern(remaining2) :: patterns
      }
    } else patterns

    patterns2 match {
      case Nil      => throw new Exception(s"Glob compilation failed for pattern: $pattern")
      case p :: Nil => p
      case _        => CompositePattern(patterns2.reverse)
    }

  }

  /** Pattern abstraction. */
  sealed trait Pattern {

    /*Create a single-use, mutable-state [[Matcher]].
     * Wraps both the pattern and a string argument.
     * Gives access to matching result and details.
     */
    final def matcher(value: String): Matcher =
      new LeftToRightMatcher(value, this)

  }

  /** Character check defined between brackets, either class or range. */
  sealed trait CharacterCheck {

    /** Check is given character can be accepted. */
    def check(c: Char): Boolean
  }

  /** A type of pattern with variable match length,
    * which can possibly consume nothing or all the remaining input.
    */
  sealed trait GreedyPattern extends Pattern

  /** A type of pattern matching single character only. */
  sealed trait SingleCharacterPattern extends Pattern with CharacterCheck

  /** A pattern consisting of a sequence of nested patterns. */
  final case class CompositePattern(patterns: List[Pattern]) extends Pattern

  /** A pattern matching literally, without any wildcards. */
  final case class LiteralPattern(literal: String) extends Pattern

  /** A wildcard pattern matching anything but path separator '/' character. */
  final case object AnyStringPattern extends Pattern with GreedyPattern

  /** A wildcard pattern matching anything, inluding path separator. */
  final case object AnyPathPattern extends Pattern with GreedyPattern

  /** A wildcard pattern matching any single character except path separator '/' character. */
  final object AnySingleCharacterPattern extends Pattern with SingleCharacterPattern {
    override def check(c: Char): Boolean = c != '/'
  }

  /** A wildcard pattern matching either class or range of characters. */
  final case class BracketPattern(pattern: String) extends Pattern with SingleCharacterPattern {
    private val characterCheck = CharacterCheck.compile(pattern)
    override def check(c: Char): Boolean =
      characterCheck.check(c)
  }

  /** Support for character classes and ranges. */
  final object CharacterCheck {

    /** Compile expression between the brackets into a [[CharacterCheck]]. */
    def compile(pattern: String): CharacterCheck =
      if (pattern.isEmpty()) throw new Exception("A character check pattern cannot be empty.")
      else if (pattern.head == '!') new CharacterCheck {
        val checks = compileInternal(pattern.drop(1))
        override def check(c: Char): Boolean =
          c != '/' && !checks.foldLeft(false)((a, ck) => a || ck.check(c))
      }
      else
        compileInternal(pattern) match {
          case ch :: Nil => ch
          case checks =>
            new CharacterCheck {
              override def check(c: Char): Boolean =
                c != '/' && checks.foldLeft(false)((a, ck) => a || ck.check(c))
            }
        }

    private def compileInternal(pattern: String): List[CharacterCheck] = {
      val (list, remaining, _) = pattern
        .foldLeft((List.empty[CharacterCheck], "", false)) { case ((ls, acc, isRange), c) =>
          if (c == '/')
            throw new Exception(
              "A character check range containing an explicit '/' character is syntactically incorrect."
            )
          else if (c == '-' && acc.nonEmpty)
            (
              if (acc.length() > 1) new CharacterClassCheck(acc.dropRight(1)) :: ls else ls,
              acc.takeRight(1) + '-',
              true
            )
          else if (isRange)
            (new CharacterRangeCheck(acc.head, c) :: ls, "", false)
          else
            (ls, acc + c, false)
        }
      if (remaining.nonEmpty) new CharacterClassCheck(remaining) :: list
      else list
    }
  }

  /** Checks if the character is of any of the provided characters.
    *
    * The string cannot be empty; therefore ']' can be allowed,
    * provided that it is the first character.
    * (Thus, "][!" matches the three characters '[', ']' and '!'.)
    */
  final case class CharacterClassCheck(characters: String) extends CharacterCheck {
    override def check(c: Char): Boolean =
      c != '/' && characters.contains(c)
  }

  /** Checks if the character is between provided range.
    *
    * There is one special convention: two characters separated by '-'
    * denote a range.  (Thus, "[A-Fa-f0-9]" is equivalent to
    * "[ABCDEFabcdef0123456789]".)  One may include '-' in its literal
    * meaning by making it the first or last character between the
    * brackets.  (Thus, "[]-]" matches just the two characters ']' and
    * '-', and "[--0]" matches the three characters '-', '.', '0',
    * since '/' cannot be matched.)
    */
  final case class CharacterRangeCheck(from: Char, to: Char) extends CharacterCheck {
    override def check(c: Char): Boolean =
      c != '/' && c >= from && c <= to
  }

  /** Single-use, mutable state pattern matcher. */
  sealed trait Matcher {

    /** Find if the pattern matches part of the argument string. */
    def find(): Boolean

    /** Start position of the match. (inclusive) */
    def start(): Int

    /** End position of the match (exclusive). */
    def end(): Int
  }

  private final class LeftToRightMatcher(value: String, pattern: Pattern) extends Matcher {

    private var result: Int = -1 // Result: -1 undefined, 0 failure, 1 success
    private var pos: Int = 0 // Cursor position on the left side
    private var min: Int = -1 // Leftmost index of the match
    private var max: Int = -1 // Rightmost index of the match
    private var sticky: Boolean = false // Should we check if matched regions leave no gaps? true - check, false - not

    final def start(): Int = min
    final def end(): Int = max

    final def find(): Boolean =
      if (result < 0) apply(pattern)
      else result > 0

    final def apply(b: Boolean): Boolean = {
      sticky = b
      apply(pattern)
    }

    private def apply(pattern: Pattern): Boolean = {
      if (result != 0) {
        if (debug)
          println(s">> LeftToRight sticky=$sticky pos=$pos min=$min max=$max value=$value pattern=$pattern")
        pattern match {
          case CompositePattern(patterns) =>
            applyList(patterns)

          case LiteralPattern(literal) =>
            val mark = pos
            val i = value.indexOf(literal, pos)
            if (i >= pos) {
              min =
                if (min == -1) i
                else Math.min(min, i)
              max = i + literal.length()
              pos = max
              result =
                if (sticky && i > mark) 0
                else {
                  sticky = true
                  1
                }
            } else {
              result = 0
            }

          case p: SingleCharacterPattern =>
            if (pos < value.length() && p.check(value(pos))) {
              min = if (min == -1) pos else min
              max = pos + 1
              pos = max
              result = 1
            } else {
              result = 0
            }

          case AnyStringPattern =>
            val i = value.indexOf('/', pos)
            min = if (min == -1) pos else min
            max = if (i == -1) value.length() else i
            result = if (max == value.length()) 1 else 0

          case AnyPathPattern =>
            min = if (min == -1) pos else min
            max = value.length()
            pos = max
            result = 1
        }
      }
      if (debug)
        println(s"<< $result LeftToRight sticky=$sticky pos=$pos min=$min max=$max value=$value pattern=$pattern")
      result > 0
    }

    @tailrec
    private def applyList(patterns: List[Pattern]): Unit =
      patterns match {
        case Nil => ()

        case p :: Nil =>
          apply(p)

        case (g: GreedyPattern) :: ps =>
          val mark = pos
          val m =
            new RightToLeftMatcher(value.substring(pos), CompositePattern(ps.reverse))

          if (m.find()) {
            min = if (min == -1) pos + m.start() else min
            max = if (max == -1) pos + m.end() else Math.max(max, pos + m.end())
            pos = max
            result =
              if (m.start() == 0) 1
              else {
                val value2 = value.substring(mark, mark + m.start())
                val m2 = new LeftToRightMatcher(value2, g)
                if (m2.apply(true) && m2.start() == 0 && m2.end() == value2.length()) {
                  min = Math.min(min, mark)
                  1
                } else 0
              }
          } else {
            result = 0
          }

        case p :: ps =>
          if (apply(p)) {
            applyList(ps)
          }
      }
  }

  private final class RightToLeftMatcher(value: String, pattern: Pattern) extends Matcher {

    private var result: Int = -1 // Result: -1 undefined, 0 failure, 1 success
    private var pos: Int = value.length // Cursor position on the left side
    private var min: Int = -1 // Leftmost index of the match
    private var max: Int = -1 // Rightmost index of the match
    private var sticky: Boolean = false // Should we check if matched regions leave no gaps? true - check, false - not

    final def start(): Int = min
    final def end(): Int = max

    final def find(): Boolean =
      if (result < 0) apply(pattern)
      else result > 0

    final def apply(b: Boolean): Boolean = {
      sticky = b
      apply(pattern)
    }

    private def apply(pattern: Pattern): Boolean = {
      if (result != 0) {
        if (debug)
          println(s">> RightToLeft sticky=$sticky pos=$pos min=$min max=$max value=$value pattern=$pattern")
        pattern match {
          case CompositePattern(patterns) =>
            applyList(patterns)

          case LiteralPattern(literal) =>
            val mark = pos
            val i = value.lastIndexOf(literal, pos)
            if (i >= 0) {
              max =
                if (max == -1) i + literal.length()
                else Math.max(max, i + literal.length())
              min = i
              pos = min
              result =
                if (sticky && (i + literal.length() != mark)) 0
                else {
                  sticky = true
                  1
                }
            } else {
              result = 0
            }

          case p: SingleCharacterPattern =>
            if (pos > 0 && p.check(value(pos - 1))) {
              max = if (max == -1) pos else max
              min = pos - 1
              pos = min
              result = 1
            } else {
              result = 0
            }

          case AnyStringPattern =>
            val i = value.lastIndexOf('/', pos)
            min = if (i == -1) 0 else i + 1
            max = if (max == -1) pos else max
            result = if (min == 0) 1 else 0

          case AnyPathPattern =>
            max = if (max == -1) pos else max
            min = 0
            pos = min
            result = 1
        }
      }
      if (debug)
        println(s"<< $result RightToLeft sticky=$sticky pos=$pos min=$min max=$max value=$value pattern=$pattern")
      result > 0
    }

    @tailrec
    private def applyList(patterns: List[Pattern]): Unit =
      patterns match {
        case Nil => ()

        case p :: Nil =>
          apply(p)

        case (g: GreedyPattern) :: ps =>
          val mark = pos
          val m =
            new LeftToRightMatcher(value.substring(0, pos), CompositePattern(ps.reverse))

          if (m.find()) {
            max = if (max == -1) m.end() else max
            min = if (min == -1) m.start() else Math.min(min, m.start())
            pos = min
            result =
              if (m.end() == mark) 1
              else {
                val value2 = value.substring(m.end(), mark)
                val m2 = new RightToLeftMatcher(value2, g)
                if (m2.apply(true) && m2.start() == 0 && m2.end() == value2.length()) {
                  max = Math.max(max, mark)
                  1
                } else 0
              }
          } else {
            result = 0
          }

        // restart search

        case p :: ps =>
          if (apply(p)) {
            applyList(ps)
          }
      }
  }

}
